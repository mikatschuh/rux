use crate::{
    comp,
    error::ErrorCode,
    parser::{
        binary_op::BinaryOp,
        binding_pow::{self},
        keyword::Keyword,
        tokenizing::{
            resolve_escape_sequences,
            token::{Token, TokenKind::*},
            TokenStream,
        },
        tree::{Bracket, Node, NodeBox, Note},
        unary_op::UnaryOp,
        NodeWrapper, Parser,
    },
};

impl<'src> Token<'src> {
    pub(super) fn nud<T: TokenStream<'src>>(
        self,
        state: &mut Parser<'src, T>,
        min_bp: u8,
    ) -> Option<NodeBox<'src>> {
        Some(match self.kind {
            Ident => {
                macro_rules! submit_identifier {
                    () => {{
                        let sym = state.internalizer.get(self.src);
                        state.make_node(
                            NodeWrapper::new(self.span)
                                .with_node(Node::Ident { sym, literal: None }),
                        )
                    }};
                }
                if self.src == "_" {
                    state.make_node(NodeWrapper::new(self.span).with_node(Node::Placeholder))
                } else if binding_pow::STATEMENT >= min_bp {
                    if state.tokenizer.next_is(|tok| tok.binding_pow() == Some(0)) {
                        let content = state.pop_expr(binding_pow::LABEL, self.span.end);
                        let label = state.internalizer.get(self.src);
                        state.make_node(
                            NodeWrapper::new(self.span - content.span.end)
                                .with_node(Node::Typed { label, content }),
                        )
                    } else if state.tokenizer.next_is(|tok| tok.kind == And) {
                        let content = state.pop_expr(binding_pow::LABEL, self.span.end);
                        let label = state.internalizer.get(self.src);
                        state.make_node(
                            NodeWrapper::new(self.span - content.span.end)
                                .with_node(Node::Typed { label, content }),
                        )
                    } else {
                        submit_identifier!()
                    }
                } else {
                    submit_identifier!()
                }
            }
            Literal => {
                if binding_pow::STATEMENT >= min_bp
                    && state.tokenizer.next_is(|tok| tok.binding_pow() == Some(0))
                {
                    let content = state.pop_expr(binding_pow::LABEL, self.span.end);
                    let label = state.internalizer.get(self.src);
                    state.make_node(
                        NodeWrapper::new(self.span - content.span.end)
                            .with_node(Node::Typed { label, content }),
                    )
                } else {
                    let sym = state.internalizer.get(self.src);
                    let literal = state.tokenizer.get_literal();
                    state.make_node(NodeWrapper::new(self.span).with_node(Node::Ident {
                        sym,
                        literal: Some(literal),
                    }))
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
                    If | Loop => state.parse_if(self.span),
                    Proc => {
                        /*
                        let convention = if let Some(tok) = state
                            .tokenizer
                            .next_if(|next| next.binding_pow() == Some(0))
                        {
                            let content = state.parse_expr(0);
                            content
                        } else {
                            None
                        };
                        let Some(Token { span, kind, .. }) = state.tokenizer.next() else {
                            state
                                .errors
                                .push(self.span.end(), ErrorCode::ExpectedInterface);
                            let return_type = state
                                .make_node(NodeWrapper::new(self.span.end()).with_node(Node::Unit));
                            let body = state
                                .make_node(NodeWrapper::new(self.span.end()).with_node(Node::Unit));
                            return Some(state.make_node(NodeWrapper::new(self.span).with_node(
                                Node::Proc {
                                    interface: FunctionInterface {
                                        parameters: vec![],
                                        return_type,
                                    },
                                    convention: None,
                                    body: Path { node: body },
                                },
                            )));
                        };
                        let op = match kind {
                            Open(Bracket::Round) => BinaryOp::App,
                            Open(Bracket::Squared) => BinaryOp::Index,
                            _ => {
                                state.errors.push(span, ErrorCode::ExpectedInterface);

                            }
                        };

                        if let Some(Token { span, kind, .. }) = state.tokenizer.next_if(|tok| {
                            matches!(tok.kind, Open(Bracket::Round | Bracket::Squared))
                        }) {}
                        state.brackets += 1;
                        let op = match bracket {
                            Bracket::Round => BinaryOp::App,
                            Bracket::Squared => BinaryOp::Index,
                            _ => unreachable!(),
                        };
                        let content = state.parse_expr(0).unwrap_or_else(|| {
                            state.make_node(
                                NodeWrapper::new(self.span.end() + 1).with_node(Node::Unit),
                            )
                        });

                        let content = state.parse_list(content);
                        let end = state.handle_closed_bracket(self.span.end, bracket);
                        state.make_node(NodeWrapper::new(self.span - end).with_node(
                            Node::Binary {
                                op,
                                lhs,
                                rhs: content,
                            },
                        ));
                        let body = state.pop_path(pos);
                        state.make_node(
                            NodeWrapper::new(self.span - body.node.span)
                                .with_node(Node::Proc { convention, body }),
                        )*/
                        todo!()
                    }
                    Else => {
                        let body = state.pop_path(self.span.end);
                        state
                            .errors
                            .push(self.span - body.node.span, ErrorCode::LonelyElse);
                        return None;
                    }
                    Continue => state.parse_continue(self.span),
                    Break => state.parse_break(self.span),
                    Return => state.parse_return(self.span),
                }
            }
            Tick => {
                let (ident_span, sym) = state.next_identifier(self.span.end);
                state.make_node(
                    NodeWrapper::new(self.span.start - ident_span).with_node(Node::Lifetime(sym)),
                )
            }
            Open(own_bracket) => {
                state.brackets += 1;
                let Some(content) = state.parse_expr(0) else {
                    let end = state.handle_closed_bracket(self.span.end, own_bracket);
                    return Some(
                        state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Unit)),
                    );
                };

                let mut content = state.parse_list(content);

                let end = state.handle_closed_bracket(self.span.end, own_bracket);
                content.span = self.span - end;
                if (binding_pow::COLON >= min_bp || min_bp == binding_pow::LABEL)
                    && state.tokenizer.next_is(|tok| tok.binding_pow() == Some(0))
                {
                    let content = state.pop_expr(binding_pow::LABEL, self.span.end);
                    let label = state.internalizer.get(self.src);
                    state.make_node(
                        NodeWrapper::new(self.span - content.span.end)
                            .with_node(Node::Typed { label, content }),
                    )
                } else {
                    content
                }
            }
            Pipe => {
                let op = BinaryOp::BitOr;
                let Some(rhs) = state.parse_expr(op.binding_pow()) else {
                    return Some(
                        state.make_node(NodeWrapper::new(self.span).with_node(Node::Or(vec![]))),
                    );
                };
                let mut chain = vec![rhs];
                while state.tokenizer.next_is(|tok| tok.kind == Pipe) {
                    let Token { span, .. } = state.tokenizer.next().unwrap();
                    let next = state.pop_expr(op.binding_pow(), span.end);
                    chain.push(next);
                }
                state.make_node(
                    NodeWrapper::new(self.span - chain.last().unwrap().span)
                        .with_node(Node::Or(chain)),
                )
            }
            Plus | PlusPlus | DashDash => state.pop_expr(UnaryOp::Neg.binding_pow(), self.span.end),
            _ => match self.as_prefix() {
                Some(op) => {
                    let val = state.pop_expr(op.binding_pow(), self.span.end);
                    state.make_node(NodeWrapper::new(self.span).with_node(Node::Unary { op, val }))
                }
                _ => {
                    state.tokenizer.buffer(self);
                    return None;
                }
            },
        })
    }

    pub(super) fn led<T: TokenStream<'src>>(
        self,
        lhs: NodeBox<'src>,
        state: &mut Parser<'src, T>,
    ) -> Result<NodeBox<'src>, NodeBox<'src>> {
        Ok(match self.kind {
            Open(bracket) if matches!(bracket, Bracket::Round | Bracket::Squared) => {
                state.brackets += 1;
                let op = match bracket {
                    Bracket::Round => BinaryOp::App,
                    Bracket::Squared => BinaryOp::Index,
                    _ => unreachable!(),
                };
                let content = state.parse_expr(0).unwrap_or_else(|| {
                    state.make_node(NodeWrapper::new(self.span.end() + 1).with_node(Node::Unit))
                });

                let content = state.parse_list(content);
                let end = state.handle_closed_bracket(self.span.end, bracket);
                state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Binary {
                    op,
                    lhs,
                    rhs: content,
                }))
            }
            Tick => {
                let (ident_span, sym) = state.next_identifier(self.span.end);
                state.make_node(
                    NodeWrapper::new(lhs.span - ident_span)
                        .with_node(Node::Lifetimed { sym, val: lhs }),
                )
            }
            ColonColon => state.parse_dynamic_arg_op(
                lhs,
                ColonColon,
                binding_pow::WRITE_RIGHT,
                |exprs| Node::Binding { exprs },
                self.span.end + 1,
            ),
            Colon => state.parse_dynamic_arg_op(
                lhs,
                ColonColon,
                binding_pow::COLON,
                |exprs| Node::Iterator { exprs },
                self.span.end + 1,
            ),
            EqualPipe => {
                state.tokenizer.buffer(Token {
                    span: self.span.end(),
                    src: &self.src[1..2],
                    kind: Pipe,
                });
                let op = BinaryOp::Write;
                let rhs = state.pop_expr(op.binding_pow(), self.span.end);
                state.make_node(
                    NodeWrapper::new(lhs.span - rhs.span).with_node(Node::Binary { op, lhs, rhs }),
                )
            }
            Pipe => {
                let op = BinaryOp::BitOr;
                let Some(rhs) = state.parse_expr(op.binding_pow()) else {
                    return Ok(state.make_node(
                        NodeWrapper::new(lhs.span - self.span).with_node(Node::Or(vec![lhs])),
                    ));
                };
                let mut chain = vec![lhs, rhs];
                while state.tokenizer.next_is(|tok| tok.kind == Pipe) {
                    let Token { span, .. } = state.tokenizer.next().unwrap();
                    let next = state.pop_expr(op.binding_pow(), span.end);
                    chain.push(next);
                }
                state.make_node(
                    NodeWrapper::new(chain.first().unwrap().span - chain.last().unwrap().span)
                        .with_node(Node::Or(chain)),
                )
            }
            _ => {
                if let Some(op) = self.as_postfix() {
                    state.make_node(
                        NodeWrapper::new(lhs.span - self.span)
                            .with_node(Node::Unary { op, val: lhs }),
                    )
                } else if let Some(op) = self.as_infix() {
                    let rhs = state.pop_expr(op.binding_pow(), self.span.end);
                    if op.is_chained() {
                        let mut chain = comp::Vec::new([(op, rhs)]);
                        while let Some(op) = state.tokenizer.peek().and_then(|op| op.as_infix()) {
                            if op.is_chained() {
                                let Token { span, .. } = state.tokenizer.next().unwrap();
                                let rhs = state.pop_expr(op.binding_pow(), span.end);
                                chain.push((op, rhs));
                                continue;
                            }
                            break;
                        }
                        state.make_node(NodeWrapper::new(lhs.span - chain.last().1.span).with_node(
                            Node::Chain {
                                first: lhs,
                                additions: chain,
                            },
                        ))
                    } else {
                        state.make_node(
                            NodeWrapper::new(lhs.span - rhs.span).with_node(Node::Binary {
                                op,
                                lhs,
                                rhs,
                            }),
                        )
                    }
                } else {
                    return Err(lhs);
                }
            }
        })
    }
}
