use std::ops::ControlFlow;

use crate::{
    comp,
    error::{ErrorCode, Errors, Position, Span},
    parser::{
        intern::{Internalizer, Symbol},
        tokenizing::TokenStream,
    },
    typing::TypeParser,
    utilities::Rc,
};
use bumpalo::{boxed::Box as BumpBox, Bump};
use tokenizing::{
    token::{Token, TokenKind},
    Tokenizer,
};
use tree::{Bracket, Node, NodeBox, NodeWrapper, Path};

pub mod binary_op;
mod binding_pow;
pub mod intern;
#[allow(dead_code)]
pub mod keyword;
mod rules;
pub mod symbol;
#[cfg(test)]
mod test;
pub mod tokenizing;
pub mod tree;
pub mod unary_op;

pub fn parse<'src>(
    text: &'src str,
    arena: &'src Bump,
    path: &'static std::path::Path,
) -> (NodeBox<'src>, Rc<Internalizer<'src>>, Rc<Errors<'src>>) {
    let errors = Rc::new(Errors::empty(path));
    let internalizer = Rc::new(Internalizer::new());
    let root = Parser::new(
        Tokenizer::new(text, errors.clone(), TypeParser::new()),
        internalizer.clone(),
        errors.clone(),
        arena,
    )
    .parse();

    (root, internalizer, errors)
}

struct Parser<'src, T: Iterator> {
    tokenizer: T,
    errors: Rc<Errors<'src>>,
    brackets: usize,

    internalizer: Rc<Internalizer<'src>>,
    type_parser: TypeParser,
    arena: &'src Bump,
    /*dash_slot: Option<Ref<'src, NodeBox<'src>>>,*/
}

impl<'src, T: TokenStream<'src>> Parser<'src, T> {
    #[inline]
    fn new(
        tokenizer: T,
        internalizer: Rc<Internalizer<'src>>,
        errors: Rc<Errors<'src>>,
        arena: &'src Bump,
    ) -> Self {
        Self {
            tokenizer,
            errors,
            brackets: 0,

            internalizer,
            type_parser: TypeParser::new(),
            arena,
        }
    }

    #[inline]
    fn make_node(&self, node: NodeWrapper<'src>) -> NodeBox<'src> {
        NodeBox::new(BumpBox::new_in(node, self.arena))
    }

    #[inline]
    fn parse(mut self) -> NodeBox<'src> {
        self.pop_expr(0, Position::beginning())
    }

    fn parse_expr(&mut self, min_bp: u8) -> Option<NodeBox<'src>> {
        let mut lhs = self.tokenizer.next()?.nud(self, min_bp)?;

        while let Some(tok) = self.tokenizer.peek() {
            if self.brackets == 0 {
                if let TokenKind::Closed(closed) = tok.kind {
                    let Token { span, .. } = self.tokenizer.next().unwrap();
                    self.errors
                        .push(span, ErrorCode::NoOpenedBracket { closed });
                    continue;
                }
            } // Generate missing opening bracket error

            let Some(bp) = tok.binding_pow() else {
                return Some(lhs);
            };
            if bp < min_bp {
                return Some(lhs);
            }

            let tok = self
                .tokenizer
                .next()
                .expect("the token-stream was peeked before");
            lhs = match tok.led(lhs, self) {
                Ok(new_lhs) => new_lhs,
                Err(old_lhs) => {
                    self.tokenizer.buffer(tok);

                    if let Some(rhs) = self.parse_expr(binding_pow::STATEMENT) {
                        let mut chain = comp::Vec::new([old_lhs, rhs]);
                        while let Some(rhs) = self.parse_expr(binding_pow::STATEMENT) {
                            chain.push(rhs)
                        }
                        self.make_node(
                            NodeWrapper::new(chain.first().span - chain.last().span)
                                .with_node(Node::Statements(chain)),
                        )
                    } else {
                        return Some(old_lhs);
                    }
                }
            };
        }
        // EOF
        Some(lhs)
    }

    #[inline]
    fn pop_expr(&mut self, min_bp: u8, pos: Position) -> NodeBox<'src> {
        let node = self.parse_expr(min_bp);
        self.expect_node(node, pos)
    }

    /// Parses statements. This functions goes through tokens one by one and trys
    /// to find the end of the current statement, if
    ///     a terminator is found, the current statement is an expression
    ///     a write operator is found, the current thing is a pattern
    ///     a function call is found, the current thing is an expression-statement
    fn parse_statements(&mut self) -> Option<NodeBox<'src>> {
        loop {}
    }

    #[inline]
    fn pop_path(&mut self, pos: Position) -> Path<'src> {
        let node = self.pop_expr(binding_pow::PATH, pos);
        let path = Path { node };
        path
    }

    fn handle_closed_bracket(&mut self, pos: Position, open_bracket: Bracket) -> Span {
        use TokenKind::*;
        if let Some(Token {
            kind: Closed(closed_bracket),
            ..
        }) = self.tokenizer.peek()
        {
            let found = *closed_bracket;
            let span = self.tokenizer.next().unwrap().span;
            self.brackets -= 1;
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
            let mut left_over_tokens = self.tokenizer.consume_while(
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
                    pos.into(),
                    ErrorCode::NoClosedBracket {
                        opened: open_bracket,
                    },
                );
            }

            if let Some(Token { span, .. }) = self.tokenizer.next() {
                self.brackets -= 1;
                return span;
            }
            pos.into()
        }
    }

    /// Pops an identifier if the next token is one. If not it generates the correct error message
    /// and leaves the token there. The position indicates were the identifier is expected to go.
    fn next_identifier(&mut self, pos: Position) -> (Span, Symbol<'src>) {
        if let Some(tok) = self
            .tokenizer
            .next_if(|tok| matches!(tok.kind, TokenKind::Ident))
        {
            (tok.span, self.internalizer.get(tok.src))
        } else if let Some(tok) = self
            .tokenizer
            .next_if(|tok| matches!(tok.kind, TokenKind::Literal))
        {
            _ = self.tokenizer.get_literal();
            (tok.span, self.internalizer.get(tok.src))
        } else {
            self.errors.push(pos.into(), ErrorCode::ExpectedIdent);
            (pos.into(), self.internalizer.empty())
        }
    }

    fn parse_list(&mut self, lhs: NodeBox<'src>) -> NodeBox<'src> {
        let Some(Token { span, .. }) = self.tokenizer.next_if(|tok| tok.kind == TokenKind::Comma)
        else {
            return lhs;
        };
        self.parse_dynamic_arg_op(
            lhs,
            TokenKind::Comma,
            0,
            |exprs| Node::List(exprs),
            span.end + 1,
        )
    }

    fn parse_dynamic_arg_op(
        &mut self,
        lhs: NodeBox<'src>,
        own_tok: TokenKind,
        right_bp: u8,
        nud: impl Fn(comp::Vec<NodeBox<'src>, 2>) -> Node<'src>,
        pos: Position,
    ) -> NodeBox<'src> {
        let rhs = self.pop_expr(right_bp, pos);
        let mut exprs = comp::Vec::new([lhs, rhs]);

        while let Some(Token { span, .. }) = self.tokenizer.next_if(|tok| tok.kind == own_tok) {
            let next = self.pop_expr(right_bp, span.end + 1);
            exprs.push(next)
        }

        self.make_node(
            NodeWrapper::new(exprs.first().span - exprs.last().span).with_node(nud(exprs)),
        )
    }

    fn expect_node(&mut self, node: Option<NodeBox<'src>>, pos: Position) -> NodeBox<'src> {
        node.unwrap_or_else(|| {
            self.errors.push(pos.into(), ErrorCode::ExpectedValue);
            self.make_node(NodeWrapper::new(pos.into()))
        })
    }
}
