use crate::{
    error::ErrorCode,
    parser::{
        binding_pow,
        tokenizing::{
            token::{Token, TokenKind},
            TokenStream,
        },
        tree::{Node, NodeBox, NodeWrapper},
        Parser,
    },
};

impl<'src> Parser<'src> {
    pub(super) fn parse_label(&mut self, tokens: &mut impl TokenStream<'src>) -> NodeBox<'src> {
        let Some(Token {
            span,
            kind: TokenKind::Ident,
            src,
        }) = tokens.next()
        else {
            self.errors
                .push(tokens.current_pos().into(), ErrorCode::ExpectedIdent);
            return self.make_node(NodeWrapper::new(tokens.current_pos().into()));
        };
        let rhs = self.parse_expr(tokens, binding_pow::LABEL);
        match rhs {
            None if src.ends_with(".") => {
                todo!()
            }
            None => {
                let sym = self.internalizer.get(src);
                self.make_node(NodeWrapper::new(span).with_node(Node::Ident { sym, literal: None }))
            }
            Some(rhs) => {
                let label = self.internalizer.get(src);
                self.make_node(
                    NodeWrapper::new(span - rhs.span.end).with_node(Node::Label { label, ty: rhs }),
                )
            }
        }
    }
}
