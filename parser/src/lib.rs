use tokenizer::{
    Bracket, IntegerType, Interner, Literal, Span, Symbol,
    Token::{self},
    TokenStream,
};

use std::collections::HashMap;

mod ast;
mod binary_op;
mod binding_pow;
mod error;
mod unary_op;

pub use ast::{
    Assignment, AstBuilder, BuiltinType, ControlStruct, DeclStmt, Definition, Expr, ExprKind,
    Ident, Item, JumpStruct, Label, ScopeStmt, ScopeStmtKind, Spanned, StmtExpr, StmtExprKind,
};
pub use binary_op::BinaryOp;
pub use error::Error;
pub use unary_op::UnaryOp;

pub trait Diagnostics {
    fn add(&mut self, span: Span, err: Error);
}

pub struct ParserOutput<'src, DT: tokenizer::Diagnostics, DP: Diagnostics> {
    pub ast: AstBuilder<'src>,
    pub item_table: HashMap<Symbol, Item>,
    pub err_expr: Vec<Expr>,
    pub incomplete_bindings: Vec<IncompleteBinding>,
    pub interner: Interner<'src>,

    pub tokenizer_errors: DT,
    pub parser_errors: DP,
}

pub fn parse<'src, T: TokenStream<'src>, DP: Diagnostics>(
    token_stream: T,
    errors: DP,
) -> ParserOutput<'src, T::DiagnosticsStack, DP> {
    let mut parser = Parser::new(token_stream, errors);
    parser.parse_file();
    parser.output()
}

struct Parser<'src, D: Diagnostics, T: TokenStream<'src>> {
    tokens: T,
    graph: AstBuilder<'src>,
    symbols: HashMap<Symbol, Item>,
    err_expr: Vec<Expr>,
    incomplete_bindings: Vec<IncompleteBinding>,

    errors: D,
}

#[derive(Clone, Debug)]
pub struct IncompleteBinding {
    pub keyword: Span,
    pub definition: Definition,
}

impl<'src, D: Diagnostics, T: TokenStream<'src>> Parser<'src, D, T> {
    pub fn new(token_stream: T, errors: D) -> Self {
        Self {
            tokens: token_stream,
            graph: AstBuilder::new(),
            symbols: HashMap::new(),
            err_expr: vec![],
            incomplete_bindings: vec![],
            errors,
        }
    }

    pub fn output(self) -> ParserOutput<'src, T::DiagnosticsStack, D> {
        let (interner, tokenizer_errors) = self.tokens.into_parts();
        ParserOutput {
            ast: self.graph,
            item_table: self.symbols,
            err_expr: self.err_expr,
            incomplete_bindings: self.incomplete_bindings,
            interner,
            tokenizer_errors,
            parser_errors: self.errors,
        }
    }

    fn try_get_ident(&mut self) -> Option<Ident> {
        if let Some((Token::Ident(symbol), span)) =
            self.tokens.next_if(|tok| matches!(tok, Token::Ident(_)))
        {
            Some(Ident::from_parts(symbol, span))
        } else {
            None
        }
    }

    fn expected(&mut self, error: Error) {
        self.errors.add(self.tokens.pos(), error)
    }

    fn expected_expr(&mut self) -> Expr {
        self.expected(Error::ExpectedExpr);
        self.graph.add_err_expr(self.tokens.pos())
    }

    fn expected_stmt_expr(&mut self) -> StmtExpr {
        self.expected(Error::ExpectedExpr);
        let expr = self.graph.add_err_expr(self.tokens.pos());
        self.graph.expr_as_stmt_expr(expr)
    }

    #[cfg(any())]
    fn expected_scope_stmt(&mut self) -> ScopeStmt {
        self.expected(Error::ExpectedExpr);
        let expr = self.graph.add_err_expr(self.tokens.pos());
        let stmt_expr = self.graph.expr_as_stmt_expr(expr);
        self.graph.stmt_expr_as_scope_stmt(stmt_expr)
    }

    fn stuck_at_end(&mut self) -> bool {
        if self.tokens.peek().is_none() {
            true
        } else {
            _ = self.tokens.next();
            false
        }
    }

    pub fn parse_file(&mut self) {
        while self.tokens.peek().is_some() {
            self.tokens.consume_while_matching(&Token::Semicolon);
            self.parse_item();
        }
    }

    fn parse_item(&mut self) {
        let Some(tok) = self.tokens.peek() else {
            return;
        };
        match tok {
            Token::Let => {
                let keyword = self.tokens.advance();
                let Some(ident) = self.try_get_ident() else {
                    self.expected(Error::ExpectedIdent);

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
            Token::Fn => todo!(),
            Token::Enum => todo!(),
            Token::Struct => todo!(),

            _ => {
                if let Some(expr) = self.parse_optional_expr(0) {
                    self.err_expr.push(expr)
                } else {
                    self.expected(Error::ExpectedItemDeclaration);
                    _ = self.stuck_at_end();
                }
            }
        }
    }

    #[cfg(any())]
    fn parse_scope_stmt(&mut self) -> ScopeStmt {
        self.parse_optional_scope_stmt()
            .unwrap_or_else(|| self.expected_scope_stmt())
    }

    fn parse_optional_scope_stmt(&mut self) -> Option<ScopeStmt> {
        match self.tokens.peek()? {
            Token::Fn => {
                let keyword = self.tokens.advance();
                let function = self.parse_function(keyword);
                Some(self.graph.decl_stmt_as_scope_stmt(function))
            }
            Token::Enum => todo!(),
            Token::Struct => todo!(),

            Token::Let => {
                let let_keyword = self.tokens.advance();
                Some(self.parse_binding(let_keyword, false))
            }
            Token::Var => {
                let var_keyword = self.tokens.advance();
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
        match self.tokens.peek()? {
            Token::Ident(symbol) => {
                let ident = Ident::from_parts(*symbol, self.tokens.advance());
                Some(self.parse_name_pattern(ident))
            }
            Token::Unreachable => {
                let unreachable = self.tokens.advance();
                Some(self.graph.add_unreachable(unreachable))
            }
            Token::Continue => {
                let jump = self.parse_jump_struct();
                Some(self.graph.add_continue(jump))
            }
            Token::Break => {
                let jump = self.parse_jump_struct();
                Some(self.graph.add_break(jump))
            }
            Token::Return => {
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
        let tok = self.tokens.peek()?;
        match tok {
            Token::UnitType => {
                let span = self.tokens.advance();
                Some(self.graph.add_type(span, BuiltinType::Unit))
            }
            Token::NeverType => {
                let span = self.tokens.advance();
                Some(self.graph.add_type(span, BuiltinType::Never))
            }
            Token::BoolType => {
                let span = self.tokens.advance();
                Some(self.graph.add_type(span, BuiltinType::Bool))
            }
            Token::IntegerType(integer_type) => {
                let integer_type = *integer_type;
                let span = self.tokens.advance();

                Some(self.graph.add_type(
                    span,
                    match integer_type {
                        IntegerType::Unsigned { size } => BuiltinType::Unsigned { size },
                        IntegerType::Signed { size } => BuiltinType::Signed { size },
                    },
                ))
            }
            Token::FloatType(precision) => {
                let precision = *precision;
                let span = self.tokens.advance();
                Some(self.graph.add_type(span, BuiltinType::Float { precision }))
            }

            Token::Literal(_) => {
                let (literal, span) = self.tokens.get_literal().unwrap();
                Some(self.graph.add_literal(span, literal))
            }
            Token::Quote(_) => {
                let (quote, span) = self.tokens.get_quote().unwrap();
                Some(self.graph.add_quote(span, quote.content))
            }
            Token::Boolean(boolean) => {
                let boolean = *boolean;
                let span = self.tokens.advance();
                Some(self.graph.add_boolean(span, boolean))
            }
            Token::Ident(symbol) => {
                let ident = Ident::from_parts(*symbol, self.tokens.advance());
                Some(self.graph.add_ident(ident))
            }
            Token::Open(Bracket::Curly) => {
                let opener = self.tokens.advance();
                Some(self.parse_block(opener))
            }
            Token::Open(open_kind) => {
                let open_kind = *open_kind;
                let opener = self.tokens.advance();
                let mut expr = self
                    .parse_optional_expr(0)
                    .unwrap_or_else(|| self.graph.add_unit(opener));

                self.graph.update_start(&mut expr, opener.start);

                let closer_span = self.tokens.pos();
                let closer_kind = self.tokens.next();
                match closer_kind {
                    Some(Token::Closed(closed_kind)) if closed_kind == open_kind => {
                        self.graph.update_end(&mut expr, closer_span.end);
                        Some(expr)
                    }
                    Some(Token::Closed(closed_kind)) => {
                        self.errors.add(
                            closer_span,
                            Error::LonelyClosedBracket {
                                closed: closed_kind,
                            },
                        );
                        Some(expr)
                    }
                    _ => {
                        self.errors.add(
                            closer_span,
                            Error::ExpectedClosedBracket { opened: open_kind },
                        );
                        Some(expr)
                    }
                }
            }
            Token::If => {
                let keyword = self.tokens.advance();
                let condition = self.parse_expr(0);
                let when_body = self.parse_stmt_expr();
                let else_clause = self
                    .tokens
                    .try_get(&Token::Else)
                    .map(|keyword| ControlStruct {
                        keyword,
                        body: self.parse_stmt_expr(),
                    });
                Some(
                    self.graph
                        .add_if(keyword, condition, when_body, else_clause),
                )
            }
            Token::AtSign => self.parse_optional_label().map(|label| {
                let body = self.parse_stmt_expr();
                self.graph.add_label(label, body)
            }),
            Token::Loop => {
                let keyword = self.tokens.advance();
                let body = self.parse_stmt_expr();
                Some(self.graph.add_loop(keyword, body))
            }

            _ => match UnaryOp::from_prefix(tok) {
                Some(op) => {
                    let span = self.tokens.advance();
                    let node = self.parse_expr(op.binding_pow());
                    Some(self.graph.add_unary(span, op, node))
                }
                None => None,
            },
        }
    }

    fn append_operators_to_expression(&mut self, mut lhs: Expr, min_bp: u8) -> Expr {
        loop {
            let Some(tok) = self.tokens.peek() else {
                return lhs;
            };
            if binding_pow::binding_pow(tok) < min_bp {
                return lhs;
            }

            if let Some(op) = BinaryOp::from_infix(tok) {
                let span = self.tokens.advance();
                let rhs = self.parse_expr(op.binding_pow());

                lhs = self.graph.add_binary(span, op, lhs, rhs);
            } else if let Some(op) = UnaryOp::from_postfix(tok) {
                let span = self.tokens.advance();
                lhs = self.graph.add_unary(span, op, lhs)
            } else if let Some(span) = self.tokens.try_get(&Token::Dot) {
                let Some(ident) = self.try_get_ident() else {
                    self.expected(Error::ExpectedIdent);
                    continue;
                };
                lhs = self.graph.add_field_access(span, lhs, ident);
            } else {
                return lhs;
            }
        }
    }

    fn parse_jump_struct(&mut self) -> JumpStruct {
        JumpStruct {
            keyword: self.tokens.advance(),
            label: self.parse_optional_label(),
            value: self.parse_optional_expr(0),
        }
    }

    fn parse_optional_label(&mut self) -> Option<Label> {
        self.tokens.try_get(&Token::AtSign).and_then(|span| {
            let Some(ident) = self.try_get_ident() else {
                self.expected(Error::ExpectedIdent);
                return None;
            };

            Some(Label {
                at_sign: span,
                ident,
            })
        })
    }

    fn parse_definition(&mut self) -> Definition {
        let ty = self.parse_optional_expr(0);
        if let Some(equal) = self.tokens.try_get(&Token::Equal) {
            let value = self.parse_expr(0);
            let assignment = Assignment { equal, value };

            match ty {
                Some(ty) => Definition::Type {
                    ty,
                    assignment: Some(assignment),
                },
                None => Definition::Assignment(assignment),
            }
        } else {
            match ty {
                Some(ty) => Definition::Type {
                    ty,
                    assignment: None,
                },
                None => {
                    self.expected(Error::ExpectedAtleastType);
                    Definition::Type {
                        ty: self.graph.add_err_expr(self.tokens.pos()),
                        assignment: None,
                    }
                }
            }
        }
    }

    fn parse_block(&mut self, opener: Span) -> Expr {
        let mut stmts = vec![];
        let end = loop {
            self.tokens.consume_while_matching(&Token::Semicolon);
            if let Some(closer) = self.tokens.try_get(&Token::Closed(Bracket::Curly)) {
                break closer;
            }

            if let Some(statement) = self.parse_optional_scope_stmt() {
                stmts.push(statement);
            } else {
                self.expected(Error::ExpectedClosedBracket {
                    opened: Bracket::Curly,
                });
                if self.stuck_at_end() {
                    break self.tokens.advance();
                }
            }
        };
        if stmts.is_empty() {
            self.graph.add_unit(opener - end)
        } else {
            self.graph.add_block(opener - end, stmts.into_boxed_slice())
        }
    }

    fn parse_binding(&mut self, keyword: Span, mutable: bool) -> ScopeStmt {
        let Some(ident) = self.try_get_ident() else {
            self.expected(Error::ExpectedIdent);
            return self.graph.add_incomplete_binding(keyword);
        };

        let definition = self.parse_definition();
        self.graph.add_binding(mutable, keyword, ident, definition)
    }

    fn parse_name_pattern(&mut self, ident: Ident) -> StmtExpr {
        let Some(tok) = self.tokens.peek() else {
            let expr = self.graph.add_ident(ident);
            return self.graph.expr_as_stmt_expr(expr);
        };

        // assignments:
        if let Some(op) = BinaryOp::from_assign(tok) {
            let op_span = self.tokens.advance();
            let lhs = self.graph.add_ident(ident.clone());
            let rhs = self.parse_expr(0);
            let value = self.graph.add_binary(op_span, op, lhs, rhs);

            return self.graph.add_assignment(ident, op_span, value);
        } else if let Some(op) = BinaryOp::from_inc_or_dec(tok) {
            let op_span = self.tokens.advance();
            let lhs = self.graph.add_ident(ident.clone());
            let rhs = self.graph.add_literal(op_span, Literal::from(1));
            let value = self.graph.add_binary(op_span, op, lhs, rhs);

            return self.graph.add_assignment(ident, op_span, value);
        } else if let Some(equal) = self.tokens.try_get(&Token::Equal) {
            let value = self.parse_expr(0);
            return self.graph.add_assignment(ident, equal, value);
        }

        // if we actually didnt have a pattern here
        let lhs = self.graph.add_ident(ident);
        let value = self.append_operators_to_expression(lhs, 0);
        self.graph.expr_as_stmt_expr(value)
    }

    fn parse_function(&mut self, keyword: Span) -> DeclStmt {
        let Some(ident) = self.try_get_ident() else {
            self.expected(Error::ExpectedIdent);
            return self.graph.add_incomplete_decl(keyword);
        };
        let mut parameters = HashMap::new();
        if self.tokens.try_get(&Token::Open(Bracket::Round)).is_some() {
            _ = loop {
                self.tokens.consume_while_matching(&Token::Comma);

                if let Some(closed) = self.tokens.try_get(&Token::Closed(Bracket::Round)) {
                    break closed;
                }

                let Some(ident) = self.try_get_ident() else {
                    self.expected(Error::ExpectedIdent);
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
            self.errors.add(span, Error::ExpectedOpenParen);
        }

        let output = self.parse_expr(0);
        let body = self.parse_stmt_expr();

        self.graph
            .add_function(keyword, ident, parameters, output, body)
    }
}
