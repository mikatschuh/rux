use std::collections::HashMap;

use crate::{
    comp,
    error::{ErrorCode, Errors, Position, Span},
    parser::{
        binary_op::BinaryOp,
        const_res::{Const, ConstTable},
        intern::{Internalizer, Symbol},
        tree::{Bracket, Node, NodeBox, NodeWrapper, Scope},
        vars::{GlobalScope, LabelTable},
    },
    tokenizing::{
        token::{Token, TokenKind},
        TokenStream,
    },
    utilities::Rc,
};
use bumpalo::{boxed::Box as BumpBox, Bump};

pub mod binary_op;
mod binding_pow;
pub mod const_res;
pub mod intern;
#[allow(dead_code)]
pub mod keyword;
mod led;
mod nud;
pub mod tree;
#[allow(dead_code)]
pub mod typing;
pub mod unary_op;
pub mod vars;

pub struct Ast<'src> {
    _arena: Bump,
    pub internalizer: Rc<Internalizer<'src>>,
    pub consts: ConstTable<'src>,
    pub sym_const_table: HashMap<Symbol<'src>, Const<'src>>,
}

pub struct Parser<'src> {
    errors: Rc<Errors<'src>>,
    open_brackets: usize,

    internalizer: Rc<Internalizer<'src>>,
    arena: Bump,
}

impl<'src> Parser<'src> {
    #[inline]
    pub fn new(errors: Rc<Errors<'src>>) -> Self {
        let arena = Bump::new();
        let internalizer = Rc::new(Internalizer::new());

        Self {
            errors,
            open_brackets: 0,

            internalizer,
            arena,
        }
    }

    #[inline]
    pub fn parse(mut self, tokens: &mut impl TokenStream<'src>) -> Ast<'src> {
        let mut sym_const_table: HashMap<Symbol<'src>, Const<'src>> = HashMap::new();
        let mut consts: ConstTable = vec![];

        loop {
            let tok = tokens.peek();
            if tok.kind == TokenKind::EOF {
                break;
            }
            let (_, id) = self.pop_identifier(tokens);

            if !tokens.match_and_consume(|tok| tok.kind == TokenKind::ColonColon) {
                self.errors.push(
                    tokens.current_pos().into(),
                    ErrorCode::ExpectedItemDeclaration,
                );
            }

            let expr = self.pop_expr(tokens, &mut GlobalScope, BinaryOp::Write.binding_pow());

            _ = tokens.consume_while(|tok| tok.kind == TokenKind::Semicolon);

            sym_const_table.insert(id, Const::new(consts.len()));
            consts.push(expr)
        }

        Ast {
            _arena: self.arena,
            internalizer: self.internalizer,
            consts,
            sym_const_table,
        }
    }

    #[inline]
    fn make_node(&self, node: NodeWrapper<'src>) -> NodeBox<'src> {
        let arena_ref: &'src Bump = unsafe { std::mem::transmute(&self.arena) };
        NodeBox::new(BumpBox::new_in(node, arena_ref))
    }

    fn parse_expr(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        var_table: &mut impl LabelTable<'src>,
        min_bp: u8,
    ) -> Option<NodeBox<'src>> {
        let mut lhs = tokens.peek().nud(self, min_bp, tokens, var_table)?;

        loop {
            let tok = tokens.peek();
            if tokens.peek().kind == TokenKind::EOF {
                break;
            }

            if self.open_brackets == 0 {
                if let TokenKind::Closed(closed) = tok.kind {
                    self.errors
                        .push(tok.span, ErrorCode::NoOpenedBracket { closed });
                    tokens.consume();
                    continue;
                }
            } // Generate missing opening bracket error

            if tok.kind.is_terminator() {
                return Some(lhs);
            }
            if tok.binding_pow() < min_bp {
                return Some(lhs);
            }

            let tok = tokens.peek();
            lhs = match tok.led(lhs, self, tokens, var_table) {
                Ok(new_lhs) => new_lhs,
                Err(old_lhs) => {
                    return Some(old_lhs);
                }
            };
        }
        // EOF
        Some(lhs)
    }

    #[inline]
    fn pop_expr(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        var_table: &mut impl LabelTable<'src>,
        min_bp: u8,
    ) -> NodeBox<'src> {
        let node = self.parse_expr(tokens, var_table, min_bp);
        self.expect_node(node, tokens.current_pos())
    }

    /// Parses statements. This functions goes through tokens one by one and trys
    /// to find the end of the current statement, if
    ///     a terminator is found, the current statement is an expression
    ///     a write operator is found, the current thing is a pattern
    fn parse_statements(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        var_table: &mut impl LabelTable<'src>,
    ) -> Scope<'src> {
        let mut statements = vec![];
        loop {
            let _ = tokens.consume_while(|tok| tok.kind == TokenKind::Semicolon); // { ; <--
            if tokens.peek().kind == TokenKind::EOF {
                let last = statements.pop().unwrap_or_else(|| {
                    self.make_node(NodeWrapper::new(tokens.current_pos().into()))
                });
                return Scope {
                    statements,
                    expr: last,
                };
            }
            let stmt = self.pop_expr(tokens, var_table, binding_pow::STATEMENT);
            if tokens.match_and_consume(|tok| tok.kind == TokenKind::Semicolon) {
                statements.push(stmt);
                continue;
            }

            if tokens.peek().kind.is_terminator() {
                return Scope {
                    statements,
                    expr: stmt,
                };
            } else {
                statements.push(stmt);
            }
        }
    }

    fn handle_closed_bracket(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        open_bracket: Bracket,
    ) -> Span {
        use TokenKind::*;
        loop {
            if let Token {
                kind: Closed(closed_bracket),
                span,
                ..
            } = tokens.peek()
            {
                let found = closed_bracket;
                tokens.consume();
                self.open_brackets -= 1;
                if found != open_bracket {
                    self.errors.push(
                        span,
                        ErrorCode::WrongClosedBracket {
                            expected: open_bracket,
                            found,
                        },
                    );
                }
                return span;
            } else {
                let mut left_over_tokens =
                    tokens.consume_while(|tok| matches!(tok.kind, TokenKind::Closed(..)));

                let mut tokens_span = left_over_tokens
                    .next()
                    .map_or(tokens.current_pos().into(), |tok| tok.span);
                while let Some(Token { span, .. }) = left_over_tokens.next() {
                    tokens_span.end = span.end
                }

                if tokens.peek().kind == TokenKind::EOF {
                    self.errors.push(
                        tokens_span,
                        ErrorCode::NoClosedBracket {
                            opened: open_bracket,
                        },
                    );
                    return tokens_span;
                } else {
                    self.errors.push(
                        tokens_span,
                        ErrorCode::ExpectedClosedBracket {
                            opened: open_bracket,
                        },
                    );
                }
            }
        }
    }

    /// Pops an identifier if the next token is one. If not it generates the correct error message
    /// and leaves the token there. The position indicates were the identifier is expected to go.
    fn pop_identifier(&mut self, tokens: &mut impl TokenStream<'src>) -> (Span, Symbol<'src>) {
        let tok = tokens.peek();
        if tokens.match_and_consume(|tok| tok.kind == TokenKind::Ident) {
            (tok.span, self.internalizer.get(tok.src))
        } else if tokens.match_and_consume(|tok| matches!(tok.kind, TokenKind::Literal)) {
            (tok.span, self.internalizer.get(tok.src))
        } else {
            let pos = tokens.current_pos();
            self.errors.push(pos.into(), ErrorCode::ExpectedIdent);
            (pos.into(), self.internalizer.empty())
        }
    }

    fn parse_list(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        var_table: &mut impl LabelTable<'src>,
        lhs: NodeBox<'src>,
    ) -> NodeBox<'src> {
        if !tokens.match_and_consume(|tok| tok.kind == TokenKind::Comma) {
            return lhs;
        };
        self.parse_dynamic_arg_op(tokens, var_table, lhs, TokenKind::Comma, 0, |exprs| {
            Node::List(exprs)
        })
    }

    fn parse_dynamic_arg_op(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        var_table: &mut impl LabelTable<'src>,
        lhs: NodeBox<'src>,
        own_tok: TokenKind,
        right_bp: u8,
        nud: impl Fn(comp::Vec<NodeBox<'src>, 2>) -> Node<'src>,
    ) -> NodeBox<'src> {
        let rhs = self.pop_expr(tokens, var_table, right_bp);
        let mut exprs = comp::Vec::new([lhs, rhs]);

        while tokens.match_and_consume(|tok| tok.kind == own_tok) {
            let next = self.pop_expr(tokens, var_table, right_bp);
            exprs.push(next)
        }

        self.make_node(
            NodeWrapper::new(exprs.first().span - exprs.last().span).with_node(nud(exprs)),
        )
    }

    fn expect_node(&mut self, node: Option<NodeBox<'src>>, pos: Position) -> NodeBox<'src> {
        node.unwrap_or_else(|| {
            self.errors.push(pos.into(), ErrorCode::ExpectedExpr);
            self.make_node(NodeWrapper::new(pos.into()))
        })
    }
}
