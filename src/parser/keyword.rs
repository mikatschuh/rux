use crate::{
    error::{ErrorCode, Span},
    parser::{
        binding_pow,
        tree::{Bracket, Branch, FunctionInterface, Node, NodeBox, NodeWrapper},
        vars::{LabelTable, ParamTable, ScopedSymTable},
        Parser,
    },
    tokenizing::{
        token::{Token, TokenKind},
        TokenStream,
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
    #[inline]
    pub fn parse_control_structure(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        var_table: &mut impl LabelTable<'src>,
        mut span: Span,
        wrapper: impl FnOnce(NodeBox<'src>, NodeBox<'src>, Option<NodeBox<'src>>) -> Node<'src>,
    ) -> NodeBox<'src> {
        let condition = self.pop_expr(tokens, var_table, binding_pow::PATH);
        var_table.open_branch();
        let then_body = self.pop_expr(tokens, var_table, binding_pow::PATH);
        var_table.close_branch();

        span = span - then_body.span;

        let else_body = if tokens.match_and_consume(|x| x.kind == TokenKind::Keyword(Else)) {
            var_table.open_branch();
            let body = self.pop_expr(tokens, var_table, binding_pow::PATH);
            var_table.close_branch();

            span = span - body.span;
            Some(body)
        } else {
            None
        };
        self.make_node(NodeWrapper::new(span).with_node(wrapper(condition, then_body, else_body)))
    }

    pub fn parse_return(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        var_table: &mut impl LabelTable<'src>,
        span: Span,
    ) -> NodeBox<'src> {
        let val = self.pop_expr(tokens, var_table, binding_pow::PATH);
        self.make_node(NodeWrapper::new(span - val.span).with_node(Node::Return { val }))
    }

    pub fn parse_break(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        var_table: &mut impl LabelTable<'src>,
        span: Span,
    ) -> NodeBox<'src> {
        let mut end = span.end;
        let layers = tokens
            .consume_while(|tok| tok.kind == TokenKind::Keyword(Keyword::Break))
            .map(|tok| end = tok.span.end)
            .count();
        let val = self.pop_expr(tokens, var_table, binding_pow::PATH);
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

    pub fn parse_proc(
        &mut self,
        tokens: &mut impl TokenStream<'src>,
        _: &mut impl LabelTable<'src>,
        span: Span,
    ) -> NodeBox<'src> {
        let convention = if tokens.match_and_consume(|tok| tok.kind == TokenKind::DashDash) {
            let mut var_table = ScopedSymTable::new();
            let content = self.pop_expr(tokens, &mut var_table, binding_pow::CALL_CONVENTION);
            let var_table = var_table.output_table();
            Some(if var_table.is_empty() {
                content
            } else {
                self.make_node(
                    NodeWrapper::new(content.span)
                        .with_node(Node::Branch(Branch { var_table, content })),
                )
            })
        } else {
            None
        };

        let mut param_table = ParamTable::new();

        match tokens.peek() {
            Token {
                kind: TokenKind::Open(Bracket::Round),
                ..
            } => {
                tokens.consume();
                self.open_brackets += 1;

                loop {
                    if tokens.peek().kind == TokenKind::Closed(Bracket::Round) {
                        break;
                    }

                    let (.., ident) = self.pop_identifier(tokens);
                    let ty = self.pop_expr(tokens, &mut param_table, 0);
                    param_table
                        .new_label(span, ident, ty)
                        .unwrap_or_else(|e| self.errors.push_err(e));

                    if !tokens.match_and_consume(|tok| tok.kind == TokenKind::Comma) {
                        if !matches!(tokens.peek().kind, TokenKind::Closed(..)) {
                            self.errors
                                .push(tokens.current_pos().into(), ErrorCode::ExpectedComma);
                        }
                        break;
                    };
                }

                _ = self.handle_closed_bracket(tokens, Bracket::Round);
            }
            _ => self
                .errors
                .push(tokens.current_pos().into(), ErrorCode::ExpectedOpenParen),
        }
        let return_type = {
            let mut var_table = ScopedSymTable::new();
            let ty = self.pop_expr(tokens, &mut var_table, binding_pow::PATH);
            let var_table = var_table.output_table();
            if var_table.is_empty() {
                ty
            } else {
                self.make_node(NodeWrapper::new(ty.span).with_node(Node::Branch(Branch {
                    var_table,
                    content: ty,
                })))
            }
        };
        let (parameters, mut var_table) = param_table.make_scoped();
        let body = self.pop_expr(tokens, &mut var_table, binding_pow::PATH);

        self.make_node(NodeWrapper::new(span - body.span).with_node(Node::Proc {
            interface: FunctionInterface {
                parameters: parameters,
                return_type,
            },
            convention,
            var_table: var_table.output_table(),
            body,
        }))
    }
}
