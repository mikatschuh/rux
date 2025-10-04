use crate::{
    error::{ErrorCode, Span},
    parser::{
        binding_pow,
        tokenizing::{
            token::{Token, TokenKind},
            TokenStream,
        },
        tree::{Bracket, FunctionInterface, Node, NodeBox, NodeWrapper},
        Parser,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Proc,
    Struct,
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
            Struct => "struct",
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
            "struct" => Struct,
            "struktur" => Struct,
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

    pub fn parse_struct(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        span: Span,
    ) -> NodeBox<'src> {
        if let Some(Token { .. }) =
            tokens.next_if(|tok| tok.kind == TokenKind::Open(Bracket::Curly))
        {
            self.open_brackets += 1;
            let Some(content) = self.parse_expr(tokens, 0) else {
                let end = self.handle_closed_bracket(tokens, Bracket::Curly);
                return self.make_node(
                    NodeWrapper::new(span - end).with_node(Node::Struct { fields: vec![] }),
                );
            };
            let mut fields = vec![content];

            while let Some(Token { .. }) = tokens.next_if(|tok| tok.kind == TokenKind::Comma) {
                let Some(expr) = self.parse_expr(tokens, binding_pow::STATEMENT) else {
                    break;
                };
                fields.push(expr)
            }

            let end = self.handle_closed_bracket(tokens, Bracket::Curly);

            self.make_node(NodeWrapper::new(span - end).with_node(Node::Struct { fields }))
        } else {
            self.errors.push(span.end(), ErrorCode::ExpectedOpenCurly);
            self.make_node(NodeWrapper::new(span))
        }
    }

    #[allow(unused)]
    pub fn parse_proc(&mut self, tokens: &mut impl TokenStream<'src>, span: Span) -> NodeBox<'src> {
        let mut parameter_sets = vec![];
        let convention = if tokens
            .next_if(|tok| tok.kind == TokenKind::DashDash)
            .is_some()
        {
            Some(self.pop_expr(tokens, binding_pow::PATH))
        } else {
            None
        };
        loop {
            match tokens.peek() {
                Some(Token {
                    kind: TokenKind::Open(bracket),
                    ..
                }) if matches!(bracket, Bracket::Round | Bracket::Squared) => {
                    let bracket = *bracket;
                    self.open_brackets += 1;

                    let mut parameters = vec![];
                    if let Some(first) = self.parse_expr(tokens, 0) {
                        parameters.push(first);
                        while let Some(Token { .. }) =
                            tokens.next_if(|tok| tok.kind == TokenKind::Comma)
                        {
                            let Some(expr) = self.parse_expr(tokens, binding_pow::STATEMENT) else {
                                break;
                            };
                            parameters.push(expr)
                        }
                    }
                    parameter_sets.push(parameters);
                    let end = self.handle_closed_bracket(tokens, bracket);
                }
                _ => {
                    let return_type = self.pop_expr(tokens, binding_pow::PATH);
                    let body = self.pop_expr(tokens, binding_pow::PATH);
                    return self.make_node(NodeWrapper::new(span - body.span).with_node(
                        Node::Proc {
                            interface: FunctionInterface {
                                parameters: parameter_sets,
                                return_type,
                            },
                            convention: None,
                            body,
                        },
                    ));
                }
            }
        }
    }
}
