use std::collections::VecDeque;

use crate::{
    error::ErrorCode,
    parser::{
        binding_pow::{self},
        keyword::Keyword,
        tree::{Bracket, Branch, Node, NodeBox, Note},
        unary_op::UnaryOp,
        vars::{LabelTable, ParamTable, ScopedSymTable, VarTableMoc},
        NodeWrapper, Parser,
    },
    tokenizing::{
        resolve_escape_sequences,
        slicing::TokenBuffer,
        token::{Token, TokenKind::*},
        TokenStream,
    },
};

impl<'src> Token<'src> {
    pub(super) fn nud<L: LabelTable<'src>>(
        self,
        state: &mut Parser<'src>,
        min_bp: u8,
        tokens: &mut impl TokenStream<'src>,
        var_table: &mut L,
    ) -> Option<NodeBox<'src>> {
        Some(match self.kind {
            Placeholder => {
                state.make_node(NodeWrapper::new(self.span).with_node(Node::Placeholder))
            }
            Ident => {
                let sym = state.internalizer.get(self.src);
                if min_bp <= binding_pow::STATEMENT && tokens.peek().binding_pow() == 0 {
                    if let Some(ty) = state.parse_expr(
                        tokens,
                        var_table,
                        if min_bp == binding_pow::STATEMENT {
                            binding_pow::LABEL
                        } else {
                            0
                        },
                    ) {
                        var_table
                            .new_label(self.span, sym, ty)
                            .unwrap_or_else(|e| state.errors.push_err(e))
                    }
                }
                match var_table.label_used(sym) {
                    Some(var) => {
                        state.make_node(NodeWrapper::new(self.span).with_node(Node::Variable(var)))
                    }
                    None => state.make_node(
                        NodeWrapper::new(self.span).with_node(Node::Ident { sym, literal: None }),
                    ),
                }
            }
            Quote => {
                let (string, confusions) = resolve_escape_sequences(self.src);
                state.make_node(
                    NodeWrapper::new(self.span)
                        .with_node(Node::Quote(string))
                        .with_notes(
                            confusions
                                .into_iter()
                                .map(Note::EscapeSequenceConfusion)
                                .collect(),
                        ),
                )
            }
            Keyword(keyword) => {
                use Keyword::*;
                match keyword {
                    Proc => state.parse_proc(tokens, var_table, self.span),
                    If => state.parse_control_structure(
                        tokens,
                        var_table,
                        self.span,
                        |condition, then_body, else_body| Node::If {
                            condition,
                            then_body,
                            else_body,
                        },
                    ),
                    Loop => state.parse_control_structure(
                        tokens,
                        var_table,
                        self.span,
                        |condition, then_body, else_body| Node::Loop {
                            condition,
                            then_body,
                            else_body,
                        },
                    ),

                    Else => {
                        let body = state.pop_expr(tokens, &mut VarTableMoc, binding_pow::PATH);
                        state
                            .errors
                            .push(self.span - body.span, ErrorCode::LonelyElse);
                        return None;
                    }
                    Continue => state.parse_continue(tokens, self.span),
                    Break => state.parse_break(tokens, var_table, self.span),
                    Return => state.parse_return(tokens, var_table, self.span),
                }
            }
            Tick => {
                let (ident_span, sym) = state.pop_identifier(tokens);
                state.make_node(
                    NodeWrapper::new(self.span.start - ident_span).with_node(Node::Lifetime(sym)),
                )
            }
            Open(Bracket::Curly) => {
                state.open_brackets += 1;

                let mut parts = vec![];
                let mut open_brackets = 0;

                'outer: loop {
                    let mut token_buffer = TokenBuffer {
                        last_outputted_pos: tokens.current_pos(),
                        tokens: VecDeque::new(),
                    };
                    loop {
                        let tok = tokens.peek();

                        if tok.kind == EOF {
                            parts.push(token_buffer);
                            break 'outer;
                        };

                        if let Open(..) = tok.kind {
                            open_brackets += 1;
                        } else if open_brackets == 0 {
                            if let Closed(..) = tok.kind {
                                parts.push(token_buffer);
                                break 'outer;
                            } else if let Comma = tok.kind {
                                tokens.consume();
                                parts.push(token_buffer);
                                break;
                            }
                        } else if let Closed(..) = tok.kind {
                            open_brackets -= 1;
                        }

                        tokens.consume();
                        token_buffer.tokens.push_back(tok);
                    }
                }

                if parts.len() == 1 {
                    if parts[0].peek().kind == EOF {
                        let end = state.handle_closed_bracket(tokens, Bracket::Curly);

                        state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Unit))
                    } else if L::IS_SCOPED {
                        let scope = state.parse_statements(&mut parts[0], var_table);

                        let end = state.handle_closed_bracket(tokens, Bracket::Curly);

                        state.make_node(
                            NodeWrapper::new(self.span - end).with_node(Node::Scope(scope)),
                        )
                    } else {
                        let mut var_table = ScopedSymTable::new();
                        let scope = state.parse_statements(&mut parts[0], &mut var_table);
                        let end = state.handle_closed_bracket(tokens, Bracket::Curly);

                        let content = state.make_node(
                            NodeWrapper::new(self.span - end).with_node(Node::Scope(scope)),
                        );

                        state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Branch(
                            Branch {
                                var_table: var_table.output_table(),
                                content,
                            },
                        )))
                    }
                } else {
                    let mut last = parts.pop().unwrap();
                    let tok = last.peek();
                    if tok.kind != EOF {
                        let mut span = tok.span;
                        loop {
                            let tok = last.peek();
                            if tok.kind == EOF {
                                break;
                            }
                            span.end = tok.span.end
                        }
                        state.errors.push(
                            span,
                            ErrorCode::ExpectedClosedBracket {
                                opened: Bracket::Curly,
                            },
                        );
                    }
                    let mut param_table = ParamTable::new();

                    for mut tokens in parts {
                        let (span, ident) = state.pop_identifier(&mut tokens);

                        if let Some(ty) = state.parse_expr(&mut tokens, &mut param_table, min_bp) {
                            match param_table.new_label(span, ident, ty) {
                                Ok(_) => {}
                                Err(err) => state.errors.push_err(err),
                            }
                        }
                    }
                    let end = state.handle_closed_bracket(tokens, Bracket::Curly);

                    state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Struct {
                        fields: param_table.output_params(),
                    }))
                }
            }
            Open(own_bracket) => {
                state.open_brackets += 1;
                let Some(content) = state.parse_expr(tokens, var_table, 0) else {
                    let end = state.handle_closed_bracket(tokens, own_bracket);
                    return Some(
                        state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Unit)),
                    );
                };

                let mut content = state.parse_list(tokens, var_table, content);

                let end = state.handle_closed_bracket(tokens, own_bracket);
                content.span = self.span - end;

                content
            }
            Plus | PlusPlus | DashDash => {
                state.pop_expr(tokens, var_table, UnaryOp::Neg.binding_pow())
            }
            _ => match self.as_prefix() {
                Some(op) => {
                    let val = state.pop_expr(tokens, var_table, op.binding_pow());
                    state.make_node(NodeWrapper::new(self.span).with_node(Node::Unary { op, val }))
                }
                _ => {
                    return None;
                }
            },
        })
    }
}
