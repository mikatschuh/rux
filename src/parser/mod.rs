use crate::{
    error::{ErrorCode, Errors},
    literal_parsing::Literal,
    ref_count::Rc,
    tokenizing::{
        TokenStream,
        span::Span,
        token::{Bracket, TokenKind},
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

    fn peek(&self) -> Option<TokenKind> {
        self.tokens.peek().map(|tok| tok.kind)
    }

    #[must_use]
    fn advance(&mut self) -> Span {
        let pos = self.tokens.pos();
        self.tokens.consume();
        pos
    }

    fn get_ident(&mut self) -> Ident {
        let tok = self.tokens.peek().unwrap();
        self.tokens.consume();
        self.interner.get_ident(tok.span, tok.src)
    }

    fn try_get_ident(&mut self) -> Option<Ident> {
        if self.peek().is_some_and(|tok| tok == TokenKind::Ident) {
            Some(self.get_ident())
        } else {
            None
        }
    }

    fn try_get(&mut self, kind: TokenKind) -> Option<Span> {
        if self.peek().is_some_and(|tok| tok == kind) {
            Some(self.advance())
        } else {
            None
        }
    }

    fn expected(&mut self, error: ErrorCode) {
        let pos = self.tokens.pos();
        self.errors.push(pos, error)
    }

    fn expected_expr(&mut self) -> Expr {
        self.expected(ErrorCode::ExpectedExpr);
        self.graph.add_err_expr(self.tokens.pos())
    }

    fn expected_stmt_expr(&mut self) -> StmtExpr {
        self.expected(ErrorCode::ExpectedExpr);
        let expr = self.graph.add_err_expr(self.tokens.pos());
        self.graph.expr_as_stmt_expr(expr)
    }

    fn expected_scope_stmt(&mut self) -> ScopeStmt {
        self.expected(ErrorCode::ExpectedExpr);
        let expr = self.graph.add_err_expr(self.tokens.pos());
        let stmt_expr = self.graph.expr_as_stmt_expr(expr);
        self.graph.stmt_expr_as_scope_stmt(stmt_expr)
    }

    fn stuck_at_end(&mut self) -> bool {
        if self.peek().is_none() {
            true
        } else {
            _ = self.advance();
            false
        }
    }

    pub fn parse_file(&mut self) {
        while let Some(tok) = self.peek() {
            self.parse_item(tok);
        }
    }

    fn parse_item(&mut self, tok: TokenKind) {
        match tok {
            TokenKind::Let => {
                let keyword = self.advance();
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
                if let Some(expr) = self.parse_optional_expr(0) {
                    self.err_expr.push(expr)
                } else {
                    self.expected(ErrorCode::ExpectedItemDeclaration);
                    _ = self.stuck_at_end();
                }
            }
        }
    }

    fn parse_scope_stmt(&mut self) -> ScopeStmt {
        self.parse_optional_scope_stmt()
            .unwrap_or_else(|| self.expected_scope_stmt())
    }

    fn parse_optional_scope_stmt(&mut self) -> Option<ScopeStmt> {
        match self.peek()? {
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

            _ => self
                .parse_optional_stmt_expr()
                .map(|stmt_expr| self.graph.stmt_expr_as_scope_stmt(stmt_expr)),
        }
    }

    fn parse_stmt_expr(&mut self) -> StmtExpr {
        self.parse_optional_stmt_expr()
            .unwrap_or_else(|| self.expected_stmt_expr())
    }

    fn parse_optional_stmt_expr(&mut self) -> Option<StmtExpr> {
        match self.peek()? {
            TokenKind::Ident => {
                let ident = self.get_ident();
                Some(self.parse_name_pattern(ident))
            }
            TokenKind::Unreachable => {
                let unreachable = self.advance();
                Some(self.graph.add_unreachable(unreachable))
            }
            TokenKind::Continue => {
                let jump = self.parse_jump_struct();
                Some(self.graph.add_continue(jump))
            }
            TokenKind::Break => {
                let jump = self.parse_jump_struct();
                Some(self.graph.add_break(jump))
            }
            TokenKind::Return => {
                let jump = self.parse_jump_struct();
                Some(self.graph.add_return(jump))
            }
            _ => self
                .parse_optional_expr(0)
                .map(|expr| self.graph.expr_as_stmt_expr(expr)),
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> Expr {
        self.parse_optional_expr(min_bp)
            .unwrap_or_else(|| self.expected_expr())
    }

    fn parse_optional_expr(&mut self, min_bp: u8) -> Option<Expr> {
        self.parse_primary()
            .map(|lhs| self.append_operators_to_expression(lhs, min_bp))
    }

    fn parse_primary(&mut self) -> Option<Expr> {
        let tok = self.peek()?;
        match tok {
            TokenKind::UnitType => {
                let span = self.advance();
                Some(self.graph.add_type(span, BuiltinType::Unit))
            }
            TokenKind::NeverType => {
                let span = self.advance();
                Some(self.graph.add_type(span, BuiltinType::Never))
            }
            TokenKind::BoolType => {
                let span = self.advance();
                Some(self.graph.add_type(span, BuiltinType::Bool))
            }
            TokenKind::IntegerType => {
                let ty = self.tokens.get_type();
                let span = self.advance();
                Some(self.graph.add_type(span, ty.into()))
            }
            TokenKind::FloatType(precision) => {
                let span = self.advance();
                Some(self.graph.add_type(span, BuiltinType::Float { precision }))
            }
            TokenKind::ComplitType => {
                let span = self.advance();
                Some(self.graph.add_type(span, BuiltinType::Complit))
            }

            TokenKind::Literal => {
                let literal = self.tokens.get_literal();
                let span = self.advance();
                Some(self.graph.add_literal(span, literal))
            }
            TokenKind::Quote { .. } => {
                let quote = self.tokens.get_quote();
                let span = self.advance();
                Some(self.graph.add_quote(span, quote))
            }
            TokenKind::Boolean(boolean) => {
                let span = self.advance();
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
                let expr = self
                    .parse_optional_expr(0)
                    .unwrap_or_else(|| self.graph.add_unit(opener));

                self.graph.update_start(expr, opener.start);

                let closer_kind = self.peek();
                let closer_span = self.advance();
                match closer_kind {
                    Some(TokenKind::Closed(closed_kind)) if closed_kind == open_kind => {
                        self.graph.update_end(expr, closer_span.end);
                        Some(expr)
                    }
                    Some(TokenKind::Closed(closed_kind)) => {
                        self.errors.push(
                            closer_span,
                            ErrorCode::LonelyClosedBracket {
                                closed: closed_kind,
                            },
                        );
                        Some(expr)
                    }
                    _ => {
                        self.errors.push(
                            closer_span,
                            ErrorCode::ExpectedClosedBracket { opened: open_kind },
                        );
                        Some(expr)
                    }
                }
            }
            TokenKind::If => {
                let keyword = self.advance();
                let condition = self.parse_expr(0);
                let when_body = self.parse_stmt_expr();
                let else_clause = self.try_get(TokenKind::Else).map(|keyword| ControlStruct {
                    keyword,
                    body: self.parse_stmt_expr(),
                });
                Some(
                    self.graph
                        .add_if(keyword, condition, when_body, else_clause),
                )
            }
            TokenKind::AtSign => self.parse_optional_label().map(|label| {
                let body = self.parse_stmt_expr();
                self.graph.add_label(label, body)
            }),
            TokenKind::Loop => {
                let keyword = self.advance();
                let body = self.parse_stmt_expr();
                Some(self.graph.add_loop(keyword, body))
            }

            TokenKind::Fn => {
                let keyword = self.advance();
                Some(self.parse_function(keyword))
            }

            _ => match tok.as_prefix() {
                Some(op) => {
                    let span = self.advance();
                    let node = self.parse_expr(op.binding_pow());
                    Some(self.graph.add_unary(span, op, node))
                }
                None => None,
            },
        }
    }

    fn append_operators_to_expression(&mut self, mut lhs: Expr, min_bp: u8) -> Expr {
        loop {
            let Some(tok) = self.peek() else { return lhs };
            if tok.binding_pow() < min_bp {
                return lhs;
            }

            if let Some(op) = tok.as_infix() {
                let span = self.advance();
                let rhs = self.parse_expr(op.binding_pow());

                lhs = self.graph.add_binary(span, op, lhs, rhs);
            } else if let Some(op) = tok.as_postfix() {
                let span = self.advance();
                lhs = self.graph.add_unary(span, op, lhs)
            } else {
                return lhs;
            }
        }
    }

    fn parse_jump_struct(&mut self) -> JumpStruct {
        JumpStruct {
            keyword: self.advance(),
            label: self.parse_optional_label(),
            value: self.parse_optional_expr(0),
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
            let ty = self.parse_optional_expr(0);
            Definition::Assignment {
                ty,
                assignment: Assignment { equal, value },
            }
        } else {
            if let Some(ty) = self.parse_optional_expr(0) {
                Definition::Type(ty)
            } else {
                self.expected(ErrorCode::ExpectedType);
                Definition::Type(self.graph.add_err_expr(self.tokens.pos()))
            }
        }
    }

    fn parse_block(&mut self, opener: Span) -> Expr {
        let mut stmts = vec![];
        let end = loop {
            while self.peek().is_some_and(|tok| tok == TokenKind::Semicolon) {
                self.tokens.consume()
            }
            if let Some(closer) = self.try_get(TokenKind::Closed(Bracket::Curly)) {
                break closer;
            }

            if let Some(statement) = self.parse_optional_scope_stmt() {
                stmts.push(statement);
            } else {
                self.expected(ErrorCode::ExpectedClosedBracket {
                    opened: Bracket::Curly,
                });
                if self.stuck_at_end() {
                    break self.advance();
                }
            }
        };
        if let Some(stmts) = NonEmpty::from_vec(stmts) {
            self.graph.add_block(opener - end, stmts)
        } else {
            self.graph.add_unit(opener - end)
        }
    }

    fn parse_binding(&mut self, keyword: Span, mutable: bool) -> ScopeStmt {
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
        let Some(tok) = self.peek() else {
            let expr = self.graph.add_ident(ident);
            return self.graph.expr_as_stmt_expr(expr);
        };

        // assignments:
        if let Some(equal) = self.try_get(TokenKind::Equal) {
            let value = self.parse_expr(0);
            return self.graph.add_assignment(ident, equal, value);
        } else if let Some(op) = tok.as_assign() {
            let op_span = self.advance();
            let lhs = self.graph.add_ident(ident);
            let rhs = self.parse_expr(0);
            let value = self.graph.add_binary(op_span, op, lhs, rhs);

            return self.graph.add_assignment(ident, op_span, value);
        } else if let Some(op) = tok.as_inc_or_dec() {
            let op_span = self.advance();
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

    fn parse_function(&mut self, keyword: Span) -> Expr {
        let mut parameters = HashMap::new();
        if self.try_get(TokenKind::Open(Bracket::Round)).is_some() {
            _ = loop {
                while self.peek() == Some(TokenKind::Comma) {
                    self.tokens.consume();
                }

                if let Some(closed) = self.try_get(TokenKind::Closed(Bracket::Round)) {
                    break closed;
                }

                let Some(ident) = self.try_get_ident() else {
                    self.expected(ErrorCode::ExpectedIdent);
                    if let Some(expr) = self.parse_optional_expr(0) {
                        self.err_expr.push(expr);
                    } else if self.stuck_at_end() {
                        break self.tokens.pos();
                    }
                    continue;
                };

                let ty = self
                    .parse_optional_expr(0)
                    .unwrap_or_else(|| self.graph.add_type(self.tokens.pos(), BuiltinType::Unit));
                parameters.insert(ident, ty);
            };
        } else {
            let span = self.tokens.pos();
            self.errors.push(span, ErrorCode::ExpectedOpenParen);
        }

        let output = self.parse_expr(0);
        let body = self.parse_stmt_expr();

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
