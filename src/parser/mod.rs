use crate::{
    comp,
    error::{ErrorCode, Errors, Position, Span},
    parser::{
        intern::{Internalizer, Symbol},
        tokenizing::{
            token::{Token, TokenKind},
            TokenStream, Tokenizer,
        },
        tree::{Bracket, Node, NodeBox, NodeWrapper, Scope},
        vars::{LabelTable, ScopedSymTable},
    },
    typing::TypeParser,
    utilities::Rc,
};
use bumpalo::{boxed::Box as BumpBox, Bump};

pub mod binary_op;
mod binding_pow;
pub mod intern;
#[allow(dead_code)]
pub mod keyword;
mod led;
mod nud;
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
    let global_var_table = ScopedSymTable::new();
    let root = Parser::new(internalizer.clone(), errors.clone(), arena).parse(
        Tokenizer::new(text, errors.clone(), TypeParser::new()),
        global_var_table,
    );

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
    fn parse(
        mut self,
        mut tokens: impl TokenStream<'src>,
        mut var_table: impl LabelTable<'src>,
    ) -> NodeBox<'src> {
        let global_scope = self.parse_statements(&mut tokens, &mut var_table);
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
        var_table: &mut impl LabelTable<'src>,
        min_bp: u8,
    ) -> Option<NodeBox<'src>> {
        let mut lhs = tokens.next()?.nud(self, min_bp, tokens, var_table)?;

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
            if tokens.is_empty() {
                let last = statements.pop().unwrap_or_else(|| {
                    self.make_node(NodeWrapper::new(tokens.current_pos().into()))
                });
                return Scope {
                    statements,
                    expr: last,
                };
            }
            let stmt = self.pop_expr(tokens, var_table, binding_pow::STATEMENT);
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
        loop {
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

                if tokens.is_empty() {
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
        var_table: &mut impl LabelTable<'src>,
        lhs: NodeBox<'src>,
    ) -> NodeBox<'src> {
        let Some(..) = tokens.next_if(|tok| tok.kind == TokenKind::Comma) else {
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

        while let Some(..) = tokens.next_if(|tok| tok.kind == own_tok) {
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
