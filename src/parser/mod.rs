use std::collections::HashMap;

use bumpalo::Bump;
use nonempty::NonEmpty;

use crate::{
    error::{ErrorCode, Errors, Span},
    literal_parsing::Literal,
    tokenizing::{
        TokenStream,
        token::{Bracket, Token, TokenKind},
    },
    utilities::Rc,
};

mod ast;
mod intern;

pub use ast::{
    AstBuilder, BuiltinType, Expr, ExprKind, Label, Spanned, Stmt, StmtExpr, StmtExprKind, StmtKind,
};
pub use intern::{Interner, Symbol};

pub struct Item {
    pub ty: Option<Expr>,
    pub expr: Expr,
}

pub struct ParserOutput {
    pub arena: Bump,
    pub interner: Interner,
    pub item_table: HashMap<Symbol, Item>,
}

pub struct Parser<'tokens, 'errors, T> {
    tokens: &'tokens mut T,
    errors: Rc<Errors<'errors>>,
    graph: AstBuilder,
    interner: Interner,
    symbols: HashMap<Symbol, Item>,
}

impl<'tokens, 'errors, T: TokenStream> Parser<'tokens, 'errors, T> {
    pub fn new(token_stream: &'tokens mut T, errors: Rc<Errors<'errors>>) -> Self {
        Self {
            tokens: token_stream,
            errors,
            graph: AstBuilder::new(),
            interner: Interner::new(),
            symbols: HashMap::new(),
        }
    }

    pub fn to_symbol_table(self) -> ParserOutput {
        ParserOutput {
            arena: self.graph.to_arena(),
            interner: self.interner,
            item_table: self.symbols,
        }
    }

    fn peek(&self) -> Token {
        self.tokens.peek()
    }
    fn pos(&self) -> Span {
        self.peek().span.start.into()
    }
    fn advance(&mut self) -> Token {
        let tok = self.peek();
        self.tokens.consume();
        tok
    }

    fn get_name(&mut self) -> Spanned<Symbol> {
        let tok = self.advance();
        Spanned {
            span: tok.span,
            val: self.interner.get(tok.src),
        }
    }
    fn expect_name(&mut self) -> Spanned<Symbol> {
        if self.peek().kind == TokenKind::Ident {
            let name = self.advance();
            Spanned {
                span: name.span,
                val: self.interner.get(name.src),
            }
        } else {
            let span = self.pos();
            self.errors.push(span, ErrorCode::ExpectedIdent);
            Spanned {
                span,
                val: Symbol::unknown(),
            }
        }
    }

    fn unknown_expr(&mut self) -> Expr {
        let span = self.pos();
        self.errors.push(span, ErrorCode::ExpectedExpr);
        self.graph.add_ident(Spanned {
            span,
            val: Symbol::unknown(),
        })
    }

    pub fn parse_file(&mut self) {
        while self.peek().kind != TokenKind::Eof {
            self.parse_item();
        }
    }

    fn parse_item(&mut self) {
        match self.peek().kind {
            TokenKind::Let => {
                self.advance();
                let symbol = self.expect_name();
                let ty = self.parse_optional_expr(0);
                if self.peek().kind == TokenKind::Equal {
                    self.advance();
                }
                let value = self.parse_expr(0);
                self.symbols.insert(symbol.val, Item { ty, expr: value });
            }
            TokenKind::Fn => todo!(),
            TokenKind::Enum => todo!(),
            TokenKind::Struct => todo!(),

            _ => {
                let pos = self.peek().span.start;
                self.errors
                    .push(pos.into(), ErrorCode::ExpectedItemDeclaration);
                self.advance();
            }
        }
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.peek().kind {
            TokenKind::Semicolon => {
                self.advance();
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
                let expr = self.parse_stmt_expr();
                Some(self.graph.stmt_expr_as_stmt(expr))
            }
        }
    }

    fn parse_stmt_expr(&mut self) -> StmtExpr {
        match self.peek().kind {
            TokenKind::Ident => {
                let symbol = self.get_name();
                self.parse_name_pattern(symbol)
            }
            TokenKind::Unreachable => {
                let unreachable = self.advance().span;
                self.graph.add_unreachable(unreachable)
            }
            TokenKind::Continue => {
                let keyword = self.advance().span;
                let label = if self.peek().kind == TokenKind::Colon {
                    let colon = self.advance().span;
                    let name = self.expect_name();
                    Some(Label { colon, label: name })
                } else {
                    None
                };
                self.graph.add_continue(keyword, label)
            }
            TokenKind::Break => {
                let keyword = self.advance().span;
                let label = if self.peek().kind == TokenKind::Colon {
                    let colon = self.advance().span;
                    let name = self.expect_name();
                    Some(Label { colon, label: name })
                } else {
                    None
                };
                let value = self.parse_optional_expr(0);
                self.graph.add_break(keyword, label, value)
            }
            TokenKind::Return => {
                let keyword = self.advance().span;
                let value = self.parse_optional_expr(0);
                self.graph.add_return(keyword, value)
            }
            _ => {
                let expr = self.parse_expr(0);
                self.graph.expr_as_stmt_expr(expr)
            }
        }
    }

    fn parse_expr(&mut self, min_bp: u8) -> Expr {
        let lhs = match self.parse_primary() {
            Some(expr) => expr,
            None => self.unknown_expr(),
        };
        self.parse_expr_tail(lhs, min_bp)
    }

    fn parse_optional_expr(&mut self, min_bp: u8) -> Option<Expr> {
        let lhs = self.parse_primary()?;
        Some(self.parse_expr_tail(lhs, min_bp))
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
                let symbol = self.expect_name();
                Some(self.graph.add_ident(symbol))
            }
            TokenKind::Open(Bracket::Curly) => {
                let opener = self.advance();
                Some(self.parse_block(opener))
            }
            TokenKind::Open(opened) => {
                let opener = self.advance();
                if self.peek().kind == TokenKind::Closed(opened) {
                    let closer = self.advance().span;
                    return Some(self.graph.add_unit(opener.span - closer));
                }

                let mut expr = self.parse_expr(0);
                expr.span.start = opener.span.start;

                let closer = self.advance();
                let span = closer.span;
                match closer.kind {
                    TokenKind::Closed(closed) if closed == opened => {
                        expr.span.end = closer.span.end;
                        Some(expr)
                    }
                    TokenKind::Closed(closed) => {
                        self.errors.push(
                            span,
                            ErrorCode::WrongClosedBracket {
                                expected: opened,
                                found: closed,
                            },
                        );
                        expr.span.end = closer.span.end;
                        Some(expr)
                    }
                    _ => {
                        self.errors
                            .push(closer.span, ErrorCode::NoClosedBracket { opened });
                        expr.span.end = closer.span.end;
                        Some(expr)
                    }
                }
            }
            TokenKind::If => {
                let keyword = self.advance().span;
                let condition = self.parse_expr(0);
                let when_body = self.parse_stmt_expr();
                let else_clause = if self.peek().kind == TokenKind::Else {
                    let else_keyword = self.advance().span;
                    let else_body = self.parse_stmt_expr();
                    Some((else_keyword, else_body))
                } else {
                    None
                };
                Some(
                    self.graph
                        .add_if(keyword, condition, when_body, else_clause),
                )
            }
            TokenKind::Loop => {
                let keyword = self.advance().span;
                let body = self.parse_stmt_expr();
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

    fn parse_expr_tail(&mut self, mut lhs: Expr, min_bp: u8) -> Expr {
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
        if self.peek().kind == TokenKind::Closed(Bracket::Curly) {
            let span = self.advance().span;
            return self.graph.add_unit(opener.span - span);
        }
        let mut statements = vec![];
        loop {
            if self.peek().kind == TokenKind::Eof {
                let pos = self.pos();
                self.errors.push(
                    pos,
                    ErrorCode::NoClosedBracket {
                        opened: Bracket::Curly,
                    },
                );
                return if statements.is_empty() {
                    self.graph.add_unit(opener.span)
                } else {
                    self.graph
                        .add_block(opener.span - pos, NonEmpty::from_vec(statements).unwrap())
                };
            }
            if let Some(statement) = self.parse_stmt() {
                statements.push(statement);
            }
            if self.peek().kind == TokenKind::Closed(Bracket::Curly) {
                let span = self.advance().span;
                return if statements.is_empty() {
                    self.graph.add_unit(opener.span - span)
                } else {
                    self.graph
                        .add_block(opener.span - span, NonEmpty::from_vec(statements).unwrap())
                };
            }
        }
    }

    fn parse_binding(&mut self, keyword: Token, mutable: bool) -> Stmt {
        let keyword = keyword.span;
        let symbol = self.expect_name();

        let ty = self.parse_optional_expr(0);
        if self.peek().kind == TokenKind::Equal {
            // type inference
            let equal = self.advance();
            let value = self.parse_expr(0);

            self.graph
                .add_binding(mutable, keyword, symbol, ty, equal.span, value)
        } else {
            // unitialized
            todo!()
        }
    }

    fn parse_name_pattern(&mut self, symbol: Spanned<Symbol>) -> StmtExpr {
        // assignments:
        if self.peek().kind == TokenKind::Equal {
            let equal = self.advance();
            let value = self.parse_expr(0);
            return self.graph.add_assignment(symbol, equal.span, value);
        } else if let Some(op) = self.peek().as_assign() {
            let op_span = self.advance().span;
            let lhs = self.graph.add_ident(symbol);
            let rhs = self.parse_expr(0);
            let value = self.graph.add_binary(op_span, op, lhs, rhs);

            return self.graph.add_assignment(symbol, op_span, value);
        } else if let Some(op) = self.peek().as_inc_or_dec() {
            let op_span = self.advance().span;
            let lhs = self.graph.add_ident(symbol);
            let rhs = self.graph.add_literal(op_span, Literal::from(1));
            let value = self.graph.add_binary(op_span, op, lhs, rhs);

            return self.graph.add_assignment(symbol, op_span, value);
        }

        // if we actually didnt have a pattern here
        // labels: `name: blk`
        if self.peek().kind == TokenKind::Colon {
            let colon = self.advance().span;
            let body = self.parse_stmt_expr();
            let label = self.graph.add_label(
                Label {
                    colon,
                    label: symbol,
                },
                body,
            );
            return self.graph.expr_as_stmt_expr(label);
        }

        let lhs = self.graph.add_ident(symbol);
        let value = self.parse_expr_tail(lhs, 0);
        self.graph.expr_as_stmt_expr(value)
    }

    fn parse_function(&mut self, keyword: Token) -> Expr {
        let keyword = keyword.span;
        let mut parameters = HashMap::new();
        if let TokenKind::Open(opened) = self.peek().kind
            && opened != Bracket::Curly
        {
            self.advance();
            loop {
                if let TokenKind::Closed(closed) = self.peek().kind {
                    let span = self.advance().span;
                    if closed != opened {
                        self.errors.push(
                            span,
                            ErrorCode::WrongClosedBracket {
                                expected: opened,
                                found: closed,
                            },
                        );
                    }
                    break;
                }
                let symbol = self.expect_name();
                let ty = self
                    .parse_optional_expr(0)
                    .unwrap_or_else(|| self.graph.add_unit(self.pos()));
                parameters.insert(symbol, ty);
            }
        } else {
            let span = self.pos();
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
    use crate::{tokenizing::Tokenizer, utilities::Rc};

    fn parse(source: &'static str) -> (ParserOutput, Rc<Errors<'static>>) {
        let errors = Rc::new(Errors::empty(Path::new("example.rx")));
        let mut tokenizer = Tokenizer::new(source.as_bytes(), errors.clone(), 64);
        let mut parser = Parser::new(&mut tokenizer, errors.clone());
        parser.parse_file();
        (parser.to_symbol_table(), errors)
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
