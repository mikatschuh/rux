use crate::{
    error::Span,
    parser::{
        binding_pow,
        tokenizing::{token::TokenKind, TokenStream},
        tree::{Node, NodeBox, NodeWrapper},
        Parser,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Proc,
    Loop,
    If,
    Else,
    Continue,
    Break,
    Return,
}
use Keyword::*;

impl Keyword {
    pub fn display(&self) -> &'static str {
        match self {
            Proc => "proc",
            Loop => "loop",
            If => "if",
            Else => "else",
            Continue => "continue",
            Break => "break",
            Return => "return",
        }
    }
    pub fn from_str(string: &str) -> Option<Self> {
        Some(match string {
            "proc" => Proc,
            "prozedur" => Proc,
            "loop" => Loop,
            "wiederhole" => Loop,
            "if" => If,
            "wenn" => If,
            "else" => Else,
            "sonst" => Else,
            "continue" => Continue,
            "nächste" => Continue,
            "break" => Break,
            "verlasse" => Break,
            "return" => Return,
            "zurückgeben" => Return,
            _ => return None,
        })
    }
}

impl<'src> Parser<'src> {
    pub fn parse_if(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        mut span: Span,
    ) -> NodeBox<'src> {
        let condition = self.pop_expr(tokens, binding_pow::PATH);

        let then_body = self.pop_expr(tokens, binding_pow::PATH);
        span = span - then_body.span;
        let else_body = if let Some(..) = tokens.next_if(|x| x.kind == TokenKind::Keyword(Else)) {
            let body = self.pop_expr(tokens, binding_pow::PATH);
            span = span - body.span;
            Some(body)
        } else {
            None
        };
        self.make_node(NodeWrapper::new(span).with_node(Node::If {
            condition,
            then_body,
            else_body,
        }))
    }

    pub fn parse_loop(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        mut span: Span,
    ) -> NodeBox<'src> {
        let condition = self.pop_expr(tokens, binding_pow::PATH);

        let then_body = self.pop_expr(tokens, binding_pow::PATH);
        span = span - then_body.span;
        let else_body = if let Some(..) = tokens.next_if(|x| x.kind == TokenKind::Keyword(Else)) {
            let body = self.pop_expr(tokens, binding_pow::PATH);
            span = span - body.span;
            Some(self.pop_expr(tokens, binding_pow::PATH))
        } else {
            None
        };
        self.make_node(NodeWrapper::new(span).with_node(Node::Loop {
            condition,
            then_body,
            else_body,
        }))
    }

    pub fn parse_return(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        span: Span,
    ) -> NodeBox<'src> {
        let val = self.pop_expr(tokens, binding_pow::PATH);
        self.make_node(NodeWrapper::new(span - val.span).with_node(Node::Return { val }))
    }

    pub fn parse_break(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        span: Span,
    ) -> NodeBox<'src> {
        let mut end = span.end;
        let layers = tokens
            .consume_while(|tok| tok.kind == TokenKind::Keyword(Keyword::Break))
            .map(|tok| end = tok.span.end)
            .count();
        let val = self.pop_expr(tokens, binding_pow::PATH);
        self.make_node(NodeWrapper::new(span - val.span).with_node(Node::Break { layers, val }))
    }

    pub fn parse_continue(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        span: Span,
    ) -> NodeBox<'src> {
        let mut end = span.end;
        let layers = tokens
            .consume_while(|tok| tok.kind == TokenKind::Keyword(Keyword::Continue))
            .map(|tok| end = tok.span.end)
            .count();
        self.make_node(NodeWrapper::new(span - end).with_node(Node::Continue { layers }))
    }

    #[allow(unused)]
    pub fn parse_proc(&mut self, tokens: &mut impl TokenStream<'src>, span: Span) -> NodeBox<'src> {
        /*
        let convention = if let Some(tok) =self
            .tokenizer
            .next_if(|next| next.binding_pow() == Some(0))
        {
            let content = self.parse_expr(0);
            content
        } else {
            None
        };
        let Some(Token { span, kind, .. }) = self.tokenizer.next() else {
           self
                .errors
                .push(self.span.end(), ErrorCode::ExpectedInterface);
            let return_type =self
                .make_node(NodeWrapper::new(self.span.end()).with_node(Node::Unit));
            let body =self
                .make_node(NodeWrapper::new(self.span.end()).with_node(Node::Unit));
            return Some(self.make_node(NodeWrapper::new(self.span).with_node(
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
                self.errors.push(span, ErrorCode::ExpectedInterface);

            }
        };

        if let Some(Token { span, kind, .. }) = self.tokenizer.next_if(|tok| {
            matches!(tok.kind, Open(Bracket::Round | Bracket::Squared))
        }) {}
        self.brackets += 1;
        let op = match bracket {
            Bracket::Round => BinaryOp::App,
            Bracket::Squared => BinaryOp::Index,
            _ => unreachable!(),
        };
        let content = self.parse_expr(0).unwrap_or_else(|| {
            self.make_node(
                NodeWrapper::new(self.span.end() + 1).with_node(Node::Unit),
            )
        });

        let content = self.parse_list(content);
        let end = self.handle_closed_bracket(self.span.end, bracket);
        self.make_node(NodeWrapper::new(self.span - end).with_node(
            Node::Binary {
                op,
                lhs,
                rhs: content,
            },
        ));
        let body = self.pop_path(pos);
        self.make_node(
            NodeWrapper::new(self.span - body.node.span)
                .with_node(Node::Proc { convention, body }),
        )*/
        todo!()
    }
}
