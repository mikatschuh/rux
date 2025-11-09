use std::collections::VecDeque;

use crate::{
    error::ErrorCode,
    parser::{
        binding_pow,
        keyword::Keyword,
        tree::{Bracket, Branch, Node, NodeBox, Note},
        unary_op::UnaryOp,
        vars::{LabelTable, ParamTable, ScopedSymTable, VarTableMoc},
        NodeWrapper, Parser,
    },
    tokenizing::{
        resolve_escape_sequences, slicing::TokenBuffer, token::TokenKind::*, TokenStream,
    },
};

impl<'src> Parser<'src> {
    pub(super) fn nud<L: LabelTable<'src>>(
        &mut self,
        min_bp: u8,
        tokens: &mut impl TokenStream<'src>,
        var_table: &mut L,
    ) -> Option<NodeBox<'src>> {
        let first_tok = tokens.peek();
        Some(match first_tok.kind {
            Placeholder => {
                tokens.consume();
                self.make_node(NodeWrapper::new(first_tok.span).with_node(Node::Placeholder))
            }
            Ident => {
                tokens.consume();
                let sym = self.internalizer.get(first_tok.src);
                if min_bp <= binding_pow::STATEMENT && tokens.peek().binding_pow() == 0 {
                    if let Some(ty) = self.parse_expr(
                        tokens,
                        var_table,
                        if min_bp == binding_pow::STATEMENT {
                            binding_pow::LABEL
                        } else {
                            0
                        },
                    ) {
                        var_table
                            .new_label(first_tok.span, sym, ty)
                            .unwrap_or_else(|e| self.errors.push_err(e))
                    }
                }
                match var_table.label_used(sym) {
                    Some(var) => self
                        .make_node(NodeWrapper::new(first_tok.span).with_node(Node::Variable(var))),
                    None => self.make_node(
                        NodeWrapper::new(first_tok.span)
                            .with_node(Node::Ident { sym, literal: None }),
                    ),
                }
            }
            Quote => {
                tokens.consume();
                let (string, confusions) = resolve_escape_sequences(first_tok.src);
                self.make_node(
                    NodeWrapper::new(first_tok.span)
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
                    Proc => self.parse_proc(tokens, var_table, first_tok.span),
                    If => self.parse_control_structure(
                        tokens,
                        var_table,
                        first_tok.span,
                        |condition, then_body, else_body| Node::If {
                            condition,
                            then_body,
                            else_body,
                        },
                    ),
                    Loop => self.parse_control_structure(
                        tokens,
                        var_table,
                        first_tok.span,
                        |condition, then_body, else_body| Node::Loop {
                            condition,
                            then_body,
                            else_body,
                        },
                    ),

                    Else => {
                        let body = self.pop_expr(tokens, &mut VarTableMoc, binding_pow::PATH);
                        self.errors
                            .push(first_tok.span - body.span, ErrorCode::LonelyElse);
                        return None;
                    }
                    Continue => self.parse_continue(tokens, first_tok.span),
                    Break => self.parse_break(tokens, var_table, first_tok.span),
                    Return => self.parse_return(tokens, var_table, first_tok.span),
                }
            }
            Tick => {
                tokens.consume();
                let (ident_span, sym) = self.pop_identifier(tokens);
                self.make_node(
                    NodeWrapper::new(first_tok.span.start - ident_span)
                        .with_node(Node::Lifetime(sym)),
                )
            }
            Open(Bracket::Curly) => {
                tokens.consume();
                self.open_brackets += 1;

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
                        let end = self.handle_closed_bracket(tokens, Bracket::Curly);

                        self.make_node(NodeWrapper::new(first_tok.span - end).with_node(Node::Unit))
                    } else if L::IS_SCOPED {
                        let scope = self.parse_statements(&mut parts[0], var_table);

                        let end = self.handle_closed_bracket(tokens, Bracket::Curly);

                        self.make_node(
                            NodeWrapper::new(first_tok.span - end).with_node(Node::Scope(scope)),
                        )
                    } else {
                        let mut var_table = ScopedSymTable::new();
                        let scope = self.parse_statements(&mut parts[0], &mut var_table);
                        let end = self.handle_closed_bracket(tokens, Bracket::Curly);

                        let content = self.make_node(
                            NodeWrapper::new(first_tok.span - end).with_node(Node::Scope(scope)),
                        );

                        self.make_node(NodeWrapper::new(first_tok.span - end).with_node(
                            Node::Branch(Branch {
                                var_table: var_table.output_table(),
                                content,
                            }),
                        ))
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
                        self.errors.push(
                            span,
                            ErrorCode::ExpectedClosedBracket {
                                opened: Bracket::Curly,
                            },
                        );
                    }
                    let mut param_table = ParamTable::new();

                    for mut tokens in parts {
                        let (span, ident) = self.pop_identifier(&mut tokens);

                        if let Some(ty) = self.parse_expr(&mut tokens, &mut param_table, min_bp) {
                            match param_table.new_label(span, ident, ty) {
                                Ok(_) => {}
                                Err(err) => self.errors.push_err(err),
                            }
                        }
                    }
                    let end = self.handle_closed_bracket(tokens, Bracket::Curly);

                    self.make_node(
                        NodeWrapper::new(first_tok.span - end).with_node(Node::Struct {
                            fields: param_table.output_params(),
                        }),
                    )
                }
            }
            Open(own_bracket) => {
                tokens.consume();
                self.open_brackets += 1;
                let Some(content) = self.parse_expr(tokens, var_table, 0) else {
                    let end = self.handle_closed_bracket(tokens, own_bracket);
                    return Some(
                        self.make_node(
                            NodeWrapper::new(first_tok.span - end).with_node(Node::Unit),
                        ),
                    );
                };

                let mut content = self.parse_list(tokens, var_table, content);

                let end = self.handle_closed_bracket(tokens, own_bracket);
                content.span = first_tok.span - end;

                content
            }
            Plus | PlusPlus | DashDash => {
                tokens.consume();
                self.pop_expr(tokens, var_table, UnaryOp::Neg.binding_pow())
            }
            _ => match first_tok.as_prefix() {
                Some(op) => {
                    tokens.consume();
                    let val = self.pop_expr(tokens, var_table, op.binding_pow());
                    self.make_node(
                        NodeWrapper::new(first_tok.span).with_node(Node::Unary { op, val }),
                    )
                }
                _ => {
                    return None;
                }
            },
        })
    }
}
