use crate::{
    comp,
    parser::{
        binary_op::BinaryOp,
        binding_pow::{self},
        tree::{Bracket, Node, NodeBox},
        unary_op::UnaryOp,
        vars::LabelTable,
        NodeWrapper, Parser,
    },
    tokenizing::{
        token::{Token, TokenKind::*},
        TokenStream,
    },
};

impl<'src> Token<'src> {
    pub(super) fn led(
        self,
        lhs: NodeBox<'src>,
        state: &mut Parser<'src>,
        tokens: &mut impl TokenStream<'src>,
        var_table: &mut impl LabelTable<'src>,
    ) -> Result<NodeBox<'src>, NodeBox<'src>> {
        Ok(match self.kind {
            Open(bracket) if matches!(bracket, Bracket::Round | Bracket::Squared) => {
                state.open_brackets += 1;
                let op = match bracket {
                    Bracket::Round => BinaryOp::App,
                    Bracket::Squared => BinaryOp::Index,
                    _ => unreachable!(),
                };
                let content = state.parse_expr(tokens, var_table, 0).unwrap_or_else(|| {
                    state.make_node(NodeWrapper::new(self.span.end() + 1).with_node(Node::Unit))
                });

                let content = state.parse_list(tokens, var_table, content);
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
                var_table,
                lhs,
                ColonColon,
                binding_pow::WRITE_RIGHT,
                |exprs| Node::Binding { exprs },
            ),
            Colon => state.parse_dynamic_arg_op(
                tokens,
                var_table,
                lhs,
                ColonColon,
                binding_pow::COLON,
                |exprs| Node::Iterator { exprs },
            ),
            Dot => {
                let tok = tokens.peek();
                if tok.binding_pow() == 0 && !tok.kind.is_terminator() {
                    tokens.consume();
                    if self.span.end == tok.span.start {
                        let op = BinaryOp::FieldAccess;
                        let rhs = state.pop_expr(tokens, var_table, op.binding_pow());
                        return Ok(state.make_node(
                            NodeWrapper::new(lhs.span - rhs.span).with_node(Node::Binary {
                                op,
                                lhs,
                                rhs,
                            }),
                        ));
                    }
                }
                state.make_node(
                    NodeWrapper::new(lhs.span - self.span).with_node(Node::Unary {
                        op: UnaryOp::Deref,
                        val: lhs,
                    }),
                )
            }
            _ => {
                if let Some(op) = self.as_postfix() {
                    state.make_node(
                        NodeWrapper::new(lhs.span - self.span)
                            .with_node(Node::Unary { op, val: lhs }),
                    )
                } else if let Some(op) = self.as_infix() {
                    let rhs = state.pop_expr(tokens, var_table, op.binding_pow());
                    if op.is_chained() {
                        let mut chain = comp::Vec::new([(op, rhs)]);
                        while let Some(op) = tokens.peek().as_infix() {
                            if op.is_chained() {
                                tokens.consume();
                                let rhs = state.pop_expr(tokens, var_table, op.binding_pow());
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
