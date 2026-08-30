use crate::{
    error::{ErrorCode, Errors},
    literal_parsing::Literal,
    ref_count::Rc,
    tokenizing::{
        TokenStream,
        span::Span,
        token::{Bracket, Token, TokenKind},
    },
};
use nonempty::NonEmpty;
use std::collections::HashMap;

pub use ast::{
    Assignment, AstBuilder, BuiltinType, ControlStruct, Definition, Expr, ExprKind, Ident, Item,
    JumpStruct, Label, ScopeStmt, ScopeStmtKind, Spanned, StmtExpr, StmtExprKind,
};
pub use intern::{Interner, Symbol};

mod ast;
mod intern;

macro_rules! collect {
    ($self:expr, $expected:expr => $($body:tt)*) => {
        loop {
            $($body)*
            if $self.peek().kind == TokenKind::Eof {
                $self.expected($expected);
                break $self.pos();
            }
        }
    };
}

pub struct ParserOutput {
    pub ast: AstBuilder,
    pub interner: Interner,
    pub item_table: HashMap<Symbol, Item>,
    pub err_expr: Vec<Expr>,
    pub incomplete_bindings: Vec<IncompleteBinding>,
}

pub struct Parser<'tokens, 'errors, T> {
    tokens: &'tokens mut T,
    errors: Rc<Errors<'errors>>,
    graph: AstBuilder,
    interner: Interner,
    symbols: HashMap<Symbol, Item>,
    err_expr: Vec<Expr>,
    incomplete_bindings: Vec<IncompleteBinding>,
}

#[derive(Clone, Debug)]
pub struct IncompleteBinding {
    pub keyword: Span,
    pub definition: Definition,
}

impl<'tokens, 'errors, T: TokenStream> Parser<'tokens, 'errors, T> {
    pub fn new(token_stream: &'tokens mut T, errors: Rc<Errors<'errors>>) -> Self {
        Self {
            tokens: token_stream,
            errors,
            graph: AstBuilder::new(),
            interner: Interner::new(),
            symbols: HashMap::new(),
            err_expr: vec![],
            incomplete_bindings: vec![],
        }
    }

    pub fn output(self) -> ParserOutput {
        ParserOutput {
            ast: self.graph,
            interner: self.interner,
            item_table: self.symbols,
            err_expr: self.err_expr,
            incomplete_bindings: self.incomplete_bindings,
        }
    }

    fn peek(&self) -> Token {
        self.tokens.peek()
    }

    fn pos(&self) -> Span {
        self.peek().span.start.into()
    }

    #[must_use]
    fn advance(&mut self) -> Token {
        let tok = self.peek();
        self.tokens.consume();
        tok
    }

    fn get_ident(&mut self) -> Ident {
        let tok = self.advance();
        self.interner.get_ident(tok.span, tok.src)
    }

    fn try_get_ident(&mut self) -> Option<Ident> {
        if self.peek().kind == TokenKind::Ident {
            Some(self.get_ident())
        } else {
            None
        }
    }

    fn try_get(&mut self, kind: TokenKind) -> Option<Span> {
        if self.peek().kind == kind {
            Some(self.advance().span)
        } else {
            None
        }
    }

    fn expected(&mut self, error: ErrorCode) {
        let pos = self.pos();
        self.errors.push(pos, error)
    }

    fn stuck(&mut self) {
        _ = self.advance();
    }

    pub fn parse_file(&mut self) {
        while self.peek().kind != TokenKind::Eof {
            self.parse_item();
        }
    }

    fn parse_item(&mut self) {
        match self.peek().kind {
            TokenKind::Let => {
                let keyword = self.advance().span;
                let Some(ident) = self.try_get_ident() else {
                    self.expected(ErrorCode::ExpectedIdent);

                    let definition = self.parse_definition();
                    self.incomplete_bindings.push(IncompleteBinding {
                        keyword,
                        definition,
                    });
                    return;
                };
                let definition = self.parse_definition();
                self.symbols.insert(
                    ident.val,
                    Item::Constant {
                        keyword,
                        ident: ident.span,
                        definition,
                    },
                );
            }
            TokenKind::Fn => todo!(),
            TokenKind::Enum => todo!(),
            TokenKind::Struct => todo!(),

            _ => {
                if let Some(expr) = self.parse_optional_expr() {
                    self.err_expr.push(expr)
                } else {
                    self.expected(ErrorCode::ExpectedItemDeclaration);
                    self.stuck();
                }
            }
        }
    }

    fn parse_scope_stmt(&mut self) -> Option<ScopeStmt> {
        match self.peek().kind {
            TokenKind::Semicolon => {
                self.stuck();
                None
            }

            TokenKind::Fn => todo!(),
            TokenKind::Enum => todo!(),
            TokenKind::Struct => todo!(),

            TokenKind::Let => {
                let let_keyword = self.advance();
                Some(self.parse_binding(let_keyword, false))
            }
            TokenKind::Var => {
                let var_keyword = self.advance();
                Some(self.parse_binding(var_keyword, true))
            }

            _ => {
                let expr = self.parse_expr_stmt();
                Some(self.graph.stmt_expr_as_scope_stmt(expr))
            }
        }
    }

    fn parse_expr_stmt(&mut self) -> StmtExpr {
        match self.peek().kind {
            TokenKind::Ident => {
                let ident = self.get_ident();
                self.parse_name_pattern(ident)
            }
            TokenKind::Unreachable => {
                let unreachable = self.advance().span;
                self.graph.add_unreachable(unreachable)
            }
            TokenKind::Continue => {
                let jump = self.parse_jump_struct();
                self.graph.add_continue(jump)
            }
            TokenKind::Break => {
                let jump = self.parse_jump_struct();
                self.graph.add_break(jump)
            }
            TokenKind::Return => {
                let jump = self.parse_jump_struct();
                self.graph.add_return(jump)
            }
            _ => {
                let expr = self.parse_expr(0);
                self.graph.expr_as_stmt_expr(expr)
            }
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> Expr {
        let lhs = self
            .parse_primary()
            .unwrap_or_else(|| self.graph.add_err_expr(self.pos()));
        self.append_operators_to_expression(lhs, min_bp)
    }

    fn parse_optional_expr(&mut self) -> Option<Expr> {
        self.parse_primary()
            .map(|lhs| self.append_operators_to_expression(lhs, 0))
    }

    fn parse_jump_struct(&mut self) -> JumpStruct {
        JumpStruct {
            keyword: self.advance().span,
            label: self.parse_optional_label(),
            value: self.parse_optional_expr(),
        }
    }

    fn parse_optional_label(&mut self) -> Option<Label> {
        self.try_get(TokenKind::AtSign).and_then(|span| {
            let Some(ident) = self.try_get_ident() else {
                self.expected(ErrorCode::ExpectedIdent);
                return None;
            };

            Some(Label {
                at_sign: span,
                ident,
            })
        })
    }

    fn parse_definition(&mut self) -> Definition {
        if let Some(equal) = self.try_get(TokenKind::Equal) {
            let value = self.parse_expr(0);
            let ty = self.parse_optional_expr();
            Definition::Assignment {
                ty,
                assignment: Assignment { equal, value },
            }
        } else {
            if let Some(ty) = self.parse_optional_expr() {
                Definition::Type(ty)
            } else {
                self.expected(ErrorCode::ExpectedType);
                Definition::Type(self.graph.add_err_expr(self.pos()))
            }
        }
    }

    fn parse_primary(&mut self) -> Option<Expr> {
        match self.peek().kind {
            TokenKind::Unit => {
                let span = self.advance().span;
                Some(self.graph.add_type(span, BuiltinType::Unit))
            }
            TokenKind::Never => {
                let span = self.advance().span;
                Some(self.graph.add_type(span, BuiltinType::Never))
            }
            TokenKind::Bool => {
                let span = self.advance().span;
                Some(self.graph.add_type(span, BuiltinType::Bool))
            }
            TokenKind::IntegerType => {
                let ty = self.tokens.get_type();
                let span = self.advance().span;
                Some(self.graph.add_type(span, ty.into()))
            }
            TokenKind::Float(precision) => {
                let span = self.advance().span;
                Some(self.graph.add_type(span, BuiltinType::Float { precision }))
            }
            TokenKind::Complit => {
                let span = self.advance().span;
                Some(self.graph.add_type(span, BuiltinType::Complit))
            }

            TokenKind::Literal => {
                let literal = self.tokens.get_literal();
                let span = self.advance().span;
                Some(self.graph.add_literal(span, literal))
            }
            TokenKind::Quote { .. } => {
                let quote = self.tokens.get_quote();
                let span = self.advance().span;
                Some(self.graph.add_quote(span, quote))
            }
            TokenKind::Boolean(boolean) => {
                let span = self.advance().span;
                Some(self.graph.add_boolean(span, boolean))
            }
            TokenKind::Ident => {
                let ident = self.get_ident();
                Some(self.graph.add_ident(ident))
            }
            TokenKind::Open(Bracket::Curly) => {
                let opener = self.advance();
                Some(self.parse_block(opener))
            }
            TokenKind::Open(open_kind) => {
                let opener = self.advance();
                if self.peek().kind == TokenKind::Closed(open_kind) {
                    let closer = self.advance().span;
                    return Some(self.graph.add_unit(opener.span - closer));
                }

                let expr = self.parse_expr(0);
                self.graph.update_start(expr, opener.span.start);

                let closer = self.advance();
                let span = closer.span;
                self.graph.update_end(expr, span.end);
                match closer.kind {
                    TokenKind::Closed(closed_kind) if closed_kind == open_kind => Some(expr),
                    TokenKind::Closed(closed_kind) => {
                        self.errors.push(
                            span,
                            ErrorCode::LonelyClosedBracket {
                                closed: closed_kind,
                            },
                        );
                        Some(expr)
                    }
                    _ => {
                        self.errors.push(
                            closer.span,
                            ErrorCode::ExpectedClosedBracket { opened: open_kind },
                        );
                        Some(expr)
                    }
                }
            }
            TokenKind::If => {
                let keyword = self.advance().span;
                let condition = self.parse_expr(0);
                let when_body = self.parse_expr_stmt();
                let else_clause = self.try_get(TokenKind::Else).map(|keyword| ControlStruct {
                    keyword,
                    body: self.parse_expr_stmt(),
                });
                Some(
                    self.graph
                        .add_if(keyword, condition, when_body, else_clause),
                )
            }
            TokenKind::AtSign => self.parse_optional_label().map(|label| {
                let body = self.parse_expr_stmt();
                self.graph.add_label(label, body)
            }),
            TokenKind::Loop => {
                let keyword = self.advance().span;
                let body = self.parse_expr_stmt();
                Some(self.graph.add_loop(keyword, body))
            }

            TokenKind::Fn => {
                let keyword = self.advance();
                Some(self.parse_function(keyword))
            }

            _ => match self.peek().as_prefix() {
                Some(op) => {
                    let span = self.advance().span;
                    let node = self.parse_expr(op.binding_pow());
                    Some(self.graph.add_unary(span, op, node))
                }
                None => None,
            },
        }
    }

    fn append_operators_to_expression(&mut self, mut lhs: Expr, min_bp: u8) -> Expr {
        loop {
            if self.peek().binding_pow() < min_bp {
                return lhs;
            }

            if let Some(op) = self.peek().as_infix() {
                let span = self.advance().span;
                let rhs = self.parse_expr(op.binding_pow());

                lhs = self.graph.add_binary(span, op, lhs, rhs);
            } else if let Some(op) = self.peek().as_postfix() {
                let span = self.advance().span;
                lhs = self.graph.add_unary(span, op, lhs)
            } else {
                return lhs;
            }
        }
    }

    fn parse_block(&mut self, opener: Token) -> Expr {
        let mut stmts = vec![];
        let end = collect! {
            self, ErrorCode::ExpectedClosedBracket { opened: Bracket::Curly }

            => if let Some(closed) = self.try_get(TokenKind::Closed(Bracket::Curly)) {
                break closed;
            } else if let Some(statement) = self.parse_scope_stmt() {
                stmts.push(statement);
            } else {
                self.stuck(); // The token can't be consumed
            }
        };
        if let Some(stmts) = NonEmpty::from_vec(stmts) {
            self.graph.add_block(opener.span - end, stmts)
        } else {
            self.graph.add_unit(opener.span - end)
        }
    }

    fn parse_binding(&mut self, keyword: Token, mutable: bool) -> ScopeStmt {
        let keyword = keyword.span;
        let Some(ident) = self.try_get_ident() else {
            self.expected(ErrorCode::ExpectedIdent);

            let definition = self.parse_definition();
            return self
                .graph
                .add_incomplete_binding(mutable, keyword, definition);
        };

        let definition = self.parse_definition();
        self.graph.add_binding(mutable, keyword, ident, definition)
    }

    fn parse_name_pattern(&mut self, ident: Ident) -> StmtExpr {
        // assignments:
        if self.peek().kind == TokenKind::Equal {
            let equal = self.advance();
            let value = self.parse_expr(0);
            return self.graph.add_assignment(ident, equal.span, value);
        } else if let Some(op) = self.peek().as_assign() {
            let op_span = self.advance().span;
            let lhs = self.graph.add_ident(ident);
            let rhs = self.parse_expr(0);
            let value = self.graph.add_binary(op_span, op, lhs, rhs);

            return self.graph.add_assignment(ident, op_span, value);
        } else if let Some(op) = self.peek().as_inc_or_dec() {
            let op_span = self.advance().span;
            let lhs = self.graph.add_ident(ident);
            let rhs = self.graph.add_literal(op_span, Literal::from(1));
            let value = self.graph.add_binary(op_span, op, lhs, rhs);

            return self.graph.add_assignment(ident, op_span, value);
        }

        // if we actually didnt have a pattern here
        let lhs = self.graph.add_ident(ident);
        let value = self.append_operators_to_expression(lhs, 0);
        self.graph.expr_as_stmt_expr(value)
    }

    fn parse_function(&mut self, keyword: Token) -> Expr {
        let keyword = keyword.span;
        let mut parameters = HashMap::new();
        if self.try_get(TokenKind::Open(Bracket::Round)).is_some() {
            _ = collect! {
                self, ErrorCode::ExpectedClosedBracket { opened:Bracket::Round }

                => if let Some(closed) = self.try_get(TokenKind::Closed(Bracket::Round)) {
                    break closed;
                }

                let Some(ident) = self.try_get_ident() else {
                    if let Some(expr) = self.parse_optional_expr() {
                        self.err_expr.push(expr);
                    } else {
                        self.stuck();
                    }

                    continue;
                };

                let ty = self
                    .parse_optional_expr()
                    .unwrap_or_else(|| self.graph.add_unit(self.pos()));
                parameters.insert(ident, ty);
            };
        } else {
            let span = self.pos();
            self.errors.push(span, ErrorCode::ExpectedOpenParen);
        }

        let output = self.parse_expr(0);
        let body = self.parse_expr_stmt();

        self.graph.add_function(keyword, parameters, output, body)
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;
    use crate::{ref_count::Rc, tokenizing::Tokenizer};

    fn parse(source: &'static str) -> (ParserOutput, Rc<Errors<'static>>) {
        let errors = Rc::new(Errors::empty(Path::new("example.rx")));
        let mut tokenizer = Tokenizer::new(source, errors.clone(), 64);
        let mut parser = Parser::new(&mut tokenizer, errors.clone());
        parser.parse_file();
        (parser.output(), errors)
    }

    #[test]
    fn parses_top_level_let_item() {
        let (mut output, errors) = parse("let main = 0");
        let main = output.interner.get("main");

        assert!(output.item_table.remove(&main).is_some());
        assert_eq!(*errors, Errors::empty(Path::new("example.rx")));
    }

    #[test]
    fn recovers_after_unexpected_top_level_token() {
        let (mut output, errors) = parse("else\nlet main = 0");
        let main = output.interner.get("main");

        assert!(output.item_table.remove(&main).is_some());
        assert_ne!(*errors, Errors::empty(Path::new("example.rx")));
    }

    #[test]
    fn consumes_empty_statements_inside_blocks() {
        let (mut output, errors) = parse("let main = {; 0}");
        let main = output.interner.get("main");

        assert!(output.item_table.remove(&main).is_some());
        assert_eq!(*errors, Errors::empty(Path::new("example.rx")));
    }
}
