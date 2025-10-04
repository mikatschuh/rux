use crate::{
    comp,
    error::{ErrorCode, Errors, Position, Span},
    parser::{
        intern::{Internalizer, Symbol},
        tokenizing::TokenStream,
        tree::Scope,
    },
    typing::TypeParser,
    utilities::Rc,
};
use bumpalo::{boxed::Box as BumpBox, Bump};
use tokenizing::{
    token::{Token, TokenKind},
    Tokenizer,
};
use tree::{Bracket, Node, NodeBox, NodeWrapper};

pub mod binary_op;
mod binding_pow;
pub mod intern;
#[allow(dead_code)]
pub mod keyword;
mod rules;
pub mod tokenizing;
pub mod tree;
pub mod unary_op;
pub mod vars;

pub fn parse<'src>(
    text: &'src str,
    arena: &'src Bump,
    path: &'static std::path::Path,
) -> (NodeBox<'src>, Rc<Internalizer<'src>>, Rc<Errors<'src>>) {
    let errors = Rc::new(Errors::empty(path));
    let internalizer = Rc::new(Internalizer::new());
    let root = Parser::new(internalizer.clone(), errors.clone(), arena).parse(Tokenizer::new(
        text,
        errors.clone(),
        TypeParser::new(),
    ));

    (root, internalizer, errors)
}

struct Parser<'src> {
    errors: Rc<Errors<'src>>,
    open_brackets: usize,

    internalizer: Rc<Internalizer<'src>>,
    arena: &'src Bump,
}

impl<'src> Parser<'src> {
    #[inline]
    fn new(
        internalizer: Rc<Internalizer<'src>>,
        errors: Rc<Errors<'src>>,
        arena: &'src Bump,
    ) -> Self {
        Self {
            errors,
            open_brackets: 0,

            internalizer,
            arena,
        }
    }

    #[inline]
    fn make_node(&self, node: NodeWrapper<'src>) -> NodeBox<'src> {
        NodeBox::new(BumpBox::new_in(node, self.arena))
    }

    #[inline]
    fn parse(mut self, mut tokenizer: impl TokenStream<'src>) -> NodeBox<'src> {
        let global_scope = self.parse_statements(&mut tokenizer);
        self.make_node(
            NodeWrapper::new(
                global_scope
                    .statements
                    .first()
                    .map_or(global_scope.expr.span, |first| first.span)
                    - global_scope.expr.span,
            )
            .with_node(Node::Scope(global_scope)),
        )
    }

    fn parse_expr(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        min_bp: u8,
    ) -> Option<NodeBox<'src>> {
        let mut lhs = tokens.next()?.nud(self, min_bp, tokens)?;

        while let Some(tok) = tokens.peek() {
            if self.open_brackets == 0 {
                if let TokenKind::Closed(closed) = tok.kind {
                    let Token { span, .. } = tokens.next().unwrap();
                    self.errors
                        .push(span, ErrorCode::NoOpenedBracket { closed });
                    continue;
                }
            } // Generate missing opening bracket error

            if tok.kind.is_terminator() {
                return Some(lhs);
            }
            if tok.binding_pow() < min_bp {
                return Some(lhs);
            }

            let tok = tokens.next().expect("the token-stream was peeked before");
            lhs = match tok.led(lhs, self, tokens) {
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
    fn pop_expr(&mut self, tokens: &mut impl TokenStream<'src>, min_bp: u8) -> NodeBox<'src> {
        let node = self.parse_expr(tokens, min_bp);
        self.expect_node(node, tokens.current_pos())
    }

    /// Parses statements. This functions goes through tokens one by one and trys
    /// to find the end of the current statement, if
    ///     a terminator is found, the current statement is an expression
    ///     a write operator is found, the current thing is a pattern
    fn parse_statements(&mut self, tokens: &mut impl TokenStream<'src>) -> Scope<'src> {
        let mut statements = vec![];
        loop {
            if tokens.is_empty() {
                let last = statements.pop().unwrap_or_else(|| {
                    self.make_node(NodeWrapper::new(tokens.current_pos().into()))
                });
                return Scope {
                    statements,
                    expr: last,
                };
            }
            let stmt = self.pop_expr(tokens, binding_pow::STATEMENT);
            if tokens.next_is(|tok| tok.kind.is_terminator()) {
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
        if let Some(Token {
            kind: Closed(closed_bracket),
            ..
        }) = tokens.peek()
        {
            let found = *closed_bracket;
            let span = tokens.next().unwrap().span;
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
            /*
            if let Some(dash_slot) = self.dash_slot {
                let mut pos = span.end + 1;
                if let Some(tok) = self.tokenizer.next_if(|tok| matches!(tok.kind, Dash(..))) {
                    pos = tok.span.end + 1
                }

                if !self.tokenizer.next_is(|tok| matches!(tok.kind, Closed(..))) {
                    self.dash_slot = None;
                    let node = self
                        .parse_expr(binding_pow::SINGLE_VALUE)
                        .unwrap_node(self, pos);
                    dash_slot.write(node);
                }
            }*/
            span
        } else {
            let mut left_over_tokens = tokens.consume_while(
                |tok| !matches!(tok.kind, TokenKind::Closed(bracket) if bracket == open_bracket),
            );

            if let Some(first) = left_over_tokens.next() {
                self.errors.push(
                    first.span.start
                        - left_over_tokens
                            .last()
                            .map_or(first.span.end, |last| last.span.end),
                    ErrorCode::ExpectedClosedBracket {
                        opened: open_bracket,
                    },
                );
            } else {
                self.errors.push(
                    tokens.current_pos().into(),
                    ErrorCode::NoClosedBracket {
                        opened: open_bracket,
                    },
                );
            }

            if let Some(Token { span, .. }) = tokens.next() {
                self.open_brackets -= 1;
                return span;
            }
            tokens.current_pos().into()
        }
    }

    /// Pops an identifier if the next token is one. If not it generates the correct error message
    /// and leaves the token there. The position indicates were the identifier is expected to go.
    fn pop_identifier(&mut self, tokens: &mut impl TokenStream<'src>) -> (Span, Symbol<'src>) {
        if let Some(tok) = tokens.next_if(|tok| matches!(tok.kind, TokenKind::Ident)) {
            (tok.span, self.internalizer.get(tok.src))
        } else if let Some(tok) = tokens.next_if(|tok| matches!(tok.kind, TokenKind::Literal)) {
            _ = tokens.pop_literal();
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
        lhs: NodeBox<'src>,
    ) -> NodeBox<'src> {
        let Some(..) = tokens.next_if(|tok| tok.kind == TokenKind::Comma) else {
            return lhs;
        };
        self.parse_dynamic_arg_op(tokens, lhs, TokenKind::Comma, 0, |exprs| Node::List(exprs))
    }

    fn parse_dynamic_arg_op(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        lhs: NodeBox<'src>,
        own_tok: TokenKind,
        right_bp: u8,
        nud: impl Fn(comp::Vec<NodeBox<'src>, 2>) -> Node<'src>,
    ) -> NodeBox<'src> {
        let rhs = self.pop_expr(tokens, right_bp);
        let mut exprs = comp::Vec::new([lhs, rhs]);

        while let Some(..) = tokens.next_if(|tok| tok.kind == own_tok) {
            let next = self.pop_expr(tokens, right_bp);
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
