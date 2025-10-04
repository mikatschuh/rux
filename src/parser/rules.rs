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
    pub(super) fn nud(
        self,
        state: &mut Parser<'src>,
        min_bp: u8,
        tokens: &mut impl TokenStream<'src>,
    ) -> Option<NodeBox<'src>> {
        Some(match self.kind {
            Ident => {
                let sym = state.internalizer.get(self.src);
                if min_bp <= binding_pow::STATEMENT && tokens.next_is(|tok| tok.binding_pow() == 0)
                {
                    if let Some(ty) = state.parse_expr(tokens, binding_pow::LABEL) {
                        return Some(state.make_node(
                            NodeWrapper::new(self.span).with_node(Node::Label { label: sym, ty }),
                        ));
                    }
                }
                state.make_node(
                    NodeWrapper::new(self.span).with_node(Node::Ident { sym, literal: None }),
                )
            }
            Literal => {
                let sym = state.internalizer.get(self.src);
                let literal = tokens.pop_literal();
                state.make_node(NodeWrapper::new(self.span).with_node(Node::Ident {
                    sym,
                    literal: Some(literal),
                }))
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
                    If | Loop => state.parse_if(tokens, self.span),
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
                        let body = state.pop_expr(tokens, binding_pow::PATH);
                        state
                            .errors
                            .push(self.span - body.span, ErrorCode::LonelyElse);
                        return None;
                    }
                    Continue => state.parse_continue(tokens, self.span),
                    Break => state.parse_break(tokens, self.span),
                    Return => state.parse_return(tokens, self.span),
                }
            }
            Tick => {
                let (ident_span, sym) = state.pop_identifier(tokens);
                state.make_node(
                    NodeWrapper::new(self.span.start - ident_span).with_node(Node::Lifetime(sym)),
                )
            }
            Open(Bracket::Curly) => {
                state.brackets += 1;
                if let Some(Token { span, .. }) =
                    tokens.next_if(|tok| tok.kind == Closed(Bracket::Curly))
                {
                    return Some(
                        state.make_node(NodeWrapper::new(self.span - span).with_node(Node::Unit)),
                    );
                }
                let scope = state.parse_statements(tokens);
                let end = state.handle_closed_bracket(tokens, Bracket::Curly);
                state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Scope(scope)))
            }
            Open(own_bracket) => {
                state.brackets += 1;
                let Some(content) = state.parse_expr(tokens, 0) else {
                    let end = state.handle_closed_bracket(tokens, own_bracket);
                    return Some(
                        state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Unit)),
                    );
                };

                let mut content = state.parse_list(tokens, content);

                let end = state.handle_closed_bracket(tokens, own_bracket);
                content.span = self.span - end;

                content
            }
            Pipe => {
                let op = BinaryOp::BitOr;
                let Some(rhs) = state.parse_expr(tokens, op.binding_pow()) else {
                    return Some(
                        state.make_node(NodeWrapper::new(self.span).with_node(Node::Or(vec![]))),
                    );
                };
                let mut chain = vec![rhs];
                while tokens.next_is(|tok| tok.kind == Pipe) {
                    _ = tokens.next().unwrap();
                    let next = state.pop_expr(tokens, op.binding_pow());
                    chain.push(next);
                }
                state.make_node(
                    NodeWrapper::new(self.span - chain.last().unwrap().span)
                        .with_node(Node::Or(chain)),
                )
            }
            Plus | PlusPlus | DashDash => state.pop_expr(tokens, UnaryOp::Neg.binding_pow()),
            _ => match self.as_prefix() {
                Some(op) => {
                    let val = state.pop_expr(tokens, op.binding_pow());
                    state.make_node(NodeWrapper::new(self.span).with_node(Node::Unary { op, val }))
                }
                _ => {
                    tokens.buffer(self);
                    return None;
                }
            },
        })
    }

    pub(super) fn led(
        self,
        lhs: NodeBox<'src>,
        state: &mut Parser<'src>,
        tokens: &mut impl TokenStream<'src>,
    ) -> Result<NodeBox<'src>, NodeBox<'src>> {
        Ok(match self.kind {
            Open(bracket) if matches!(bracket, Bracket::Round | Bracket::Squared) => {
                state.brackets += 1;
                let op = match bracket {
                    Bracket::Round => BinaryOp::App,
                    Bracket::Squared => BinaryOp::Index,
                    _ => unreachable!(),
                };
                let content = state.parse_expr(tokens, 0).unwrap_or_else(|| {
                    state.make_node(NodeWrapper::new(self.span.end() + 1).with_node(Node::Unit))
                });

                let content = state.parse_list(tokens, content);
                let end = state.handle_closed_bracket(tokens, bracket);
                state.make_node(NodeWrapper::new(self.span - end).with_node(Node::Binary {
                    op,
                    lhs,
                    rhs: content,
                }))
            }
            Tick => {
                let (ident_span, sym) = state.pop_identifier(tokens);
                state.make_node(
                    NodeWrapper::new(lhs.span - ident_span)
                        .with_node(Node::Lifetimed { sym, val: lhs }),
                )
            }
            ColonColon => state.parse_dynamic_arg_op(
                tokens,
                lhs,
                ColonColon,
                binding_pow::WRITE_RIGHT,
                |exprs| Node::Binding { exprs },
            ),
            Colon => {
                state.parse_dynamic_arg_op(tokens, lhs, ColonColon, binding_pow::COLON, |exprs| {
                    Node::Iterator { exprs }
                })
            }
            Pipe => {
                let op = BinaryOp::BitOr;
                let Some(rhs) = state.parse_expr(tokens, op.binding_pow()) else {
                    return Ok(state.make_node(
                        NodeWrapper::new(lhs.span - self.span).with_node(Node::Or(vec![lhs])),
                    ));
                };
                let mut chain = vec![lhs, rhs];
                while tokens.next_is(|tok| tok.kind == Pipe) {
                    _ = tokens.next().unwrap();
                    let next = state.pop_expr(tokens, op.binding_pow());
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
                    let rhs = state.pop_expr(tokens, op.binding_pow());
                    if op.is_chained() {
                        let mut chain = comp::Vec::new([(op, rhs)]);
                        while let Some(op) = tokens.peek().and_then(|op| op.as_infix()) {
                            if op.is_chained() {
                                _ = tokens.next().unwrap();
                                let rhs = state.pop_expr(tokens, op.binding_pow());
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
