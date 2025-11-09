use crate::{
    comp,
    parser::{
        binary_op::BinaryOp,
        binding_pow,
        tree::{Bracket, Node, NodeBox},
        unary_op::UnaryOp,
        vars::LabelTable,
        NodeWrapper, Parser,
    },
    tokenizing::{token::TokenKind::*, TokenStream},
};

impl<'src> Parser<'src> {
    pub(super) fn led(
        &mut self,
        lhs: NodeBox<'src>,
        tokens: &mut impl TokenStream<'src>,
        var_table: &mut impl LabelTable<'src>,
    ) -> Result<NodeBox<'src>, NodeBox<'src>> {
        let first_tok = tokens.peek();
        Ok(match first_tok.kind {
            Open(bracket) if matches!(bracket, Bracket::Round | Bracket::Squared) => {
                tokens.consume();
                self.open_brackets += 1;
                let op = match bracket {
                    Bracket::Round => BinaryOp::App,
                    Bracket::Squared => BinaryOp::Index,
                    _ => unreachable!(),
                };
                let content = self.parse_expr(tokens, var_table, 0).unwrap_or_else(|| {
                    self.make_node(NodeWrapper::new(first_tok.span.end() + 1).with_node(Node::Unit))
                });

                let content = self.parse_list(tokens, var_table, content);
                let end = self.handle_closed_bracket(tokens, bracket);
                self.make_node(
                    NodeWrapper::new(first_tok.span - end).with_node(Node::Binary {
                        op,
                        lhs,
                        rhs: content,
                    }),
                )
            }
            Tick => {
                tokens.consume();
                let (ident_span, sym) = self.pop_identifier(tokens);
                self.make_node(
                    NodeWrapper::new(lhs.span - ident_span)
                        .with_node(Node::Lifetimed { sym, val: lhs }),
                )
            }
            ColonColon => self.parse_dynamic_arg_op(
                tokens,
                var_table,
                lhs,
                ColonColon,
                binding_pow::WRITE_RIGHT,
                |exprs| Node::Binding { exprs },
            ),
            Colon => self.parse_dynamic_arg_op(
                tokens,
                var_table,
                lhs,
                ColonColon,
                binding_pow::COLON,
                |exprs| Node::Iterator { exprs },
            ),
            Dot => {
                tokens.consume();
                let tok = tokens.peek();
                if tok.binding_pow() == 0 && !tok.kind.is_terminator() {
                    tokens.consume();
                    if first_tok.span.end == tok.span.start {
                        let op = BinaryOp::FieldAccess;
                        let rhs = self.pop_expr(tokens, var_table, op.binding_pow());
                        return Ok(self.make_node(
                            NodeWrapper::new(lhs.span - rhs.span).with_node(Node::Binary {
                                op,
                                lhs,
                                rhs,
                            }),
                        ));
                    }
                }
                self.make_node(
                    NodeWrapper::new(lhs.span - first_tok.span).with_node(Node::Unary {
                        op: UnaryOp::Deref,
                        val: lhs,
                    }),
                )
            }
            _ => {
                if let Some(op) = first_tok.as_postfix() {
                    tokens.consume();
                    self.make_node(
                        NodeWrapper::new(lhs.span - first_tok.span)
                            .with_node(Node::Unary { op, val: lhs }),
                    )
                } else if let Some(op) = first_tok.as_infix() {
                    tokens.consume();
                    let rhs = self.pop_expr(tokens, var_table, op.binding_pow());
                    if op.is_chained() {
                        let mut chain = comp::Vec::new([(op, rhs)]);
                        while let Some(op) = tokens.peek().as_infix() {
                            if op.is_chained() {
                                tokens.consume();
                                let rhs = self.pop_expr(tokens, var_table, op.binding_pow());
                                chain.push((op, rhs));
                                continue;
                            }
                            break;
                        }
                        self.make_node(NodeWrapper::new(lhs.span - chain.last().1.span).with_node(
                            Node::Chain {
                                first: lhs,
                                additions: chain,
                            },
                        ))
                    } else {
                        self.make_node(
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
