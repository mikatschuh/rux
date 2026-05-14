use std::collections::HashMap;

use bumpalo::Bump;
use nonempty::NonEmpty;

use crate::{
    error::Span,
    literal_parsing::Literal,
    parser::intern::Symbol,
    tokenizing::{binary_op::BinaryOp, token::FloatPrecision, unary_op::UnaryOp},
    type_parsing::{IntegerType, TypeSize},
    utilities::{NoDealloc, Rc},
};

pub type Stmt = Spanned<Rc<StmtKind, NoDealloc>>;
pub type StmtExpr = Spanned<Rc<StmtExprKind, NoDealloc>>;
pub type Expr = Spanned<Rc<ExprKind, NoDealloc>>;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub val: T,
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    /// Either one, type or value
    Binding {
        mutable: bool,
        keyword: Span,
        symbol: Spanned<Symbol>,
        ty: Option<Expr>,
        equal: Span,
        value: Expr,
    },
    StmtExpr {
        stmt_expr: StmtExpr,
    },
}

#[derive(Clone, Debug)]
pub struct Label {
    pub colon: Span,
    pub label: Spanned<Symbol>,
}

#[derive(Clone, Debug)]
pub enum StmtExprKind {
    Assignment {
        symbol: Spanned<Symbol>,
        equal: Span,
        value: Expr,
    },
    Unreachable,
    Continue {
        keyword: Span,
        label: Option<Label>,
    },
    Break {
        keyword: Span,
        label: Option<Label>,
        value: Option<Expr>,
    },
    Return {
        keyword: Span,
        value: Option<Expr>,
    },
    ExprStmt {
        expr: Expr,
    },
}

#[derive(Clone, Debug)]
pub enum BuiltinType {
    Unit,
    Never,

    Bool,
    Unsigned { size: TypeSize },
    Signed { size: TypeSize },
    Float { precision: FloatPrecision },
}

impl From<IntegerType> for BuiltinType {
    fn from(value: IntegerType) -> Self {
        match value {
            IntegerType::Signed { size } => Self::Signed { size },
            IntegerType::Unsigned { size } => Self::Unsigned { size },
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Ident(Symbol),

    BuiltinType(BuiltinType),
    Literal(Literal),
    Quote(String),
    Boolean(bool),
    Unit,

    Unary {
        op: Spanned<UnaryOp>,
        value: Expr,
    },
    Binary {
        lhs: Expr,
        op: Spanned<BinaryOp>,
        rhs: Expr,
    },

    Block {
        statements: NonEmpty<Stmt>,
    },

    If {
        keyword: Span,
        condition: Expr,
        when_body: StmtExpr,
        else_body: Option<(Span, StmtExpr)>,
    },
    Loop {
        keyword: Span,
        body: StmtExpr,
    },
    Label {
        label: Label,
        body: StmtExpr,
    },

    Function {
        keyword: Span,
        parameters: HashMap<Spanned<Symbol>, Expr>,
        output: Expr,
        body: StmtExpr,
    },
}

/// This one is DAG
#[derive(Debug)]
pub struct AstBuilder {
    arena: Bump,
}

impl AstBuilder {
    pub fn new() -> Self {
        Self { arena: Bump::new() }
    }

    pub fn to_arena(self) -> Bump {
        self.arena
    }

    fn push_stmt(&mut self, span: Span, kind: StmtKind) -> Stmt {
        Spanned {
            span,
            val: Rc::<StmtKind, NoDealloc>::new_in_bump(kind, &self.arena),
        }
    }
    fn push_stmt_expr(&mut self, span: Span, kind: StmtExprKind) -> StmtExpr {
        Spanned {
            span,
            val: Rc::<StmtExprKind, NoDealloc>::new_in_bump(kind, &self.arena),
        }
    }

    fn push_expr(&mut self, span: Span, kind: ExprKind) -> Expr {
        Spanned {
            span,
            val: Rc::<ExprKind, NoDealloc>::new_in_bump(kind, &self.arena),
        }
    }

    pub fn stmt_expr_as_stmt(&mut self, stmt_expr: StmtExpr) -> Stmt {
        self.push_stmt(stmt_expr.span, StmtKind::StmtExpr { stmt_expr })
    }

    pub fn expr_as_stmt(&mut self, expr: Expr) -> Stmt {
        let stmt_expr = self.expr_as_stmt_expr(expr);
        self.stmt_expr_as_stmt(stmt_expr)
    }

    pub fn expr_as_stmt_expr(&mut self, expr: Expr) -> StmtExpr {
        self.push_stmt_expr(expr.span, StmtExprKind::ExprStmt { expr })
    }

    pub fn add_binding(
        &mut self,
        mutable: bool,
        keyword: Span,
        symbol: Spanned<Symbol>,
        ty: Option<Expr>,
        equal: Span,
        value: Expr,
    ) -> Stmt {
        self.push_stmt(
            keyword - value.span,
            StmtKind::Binding {
                mutable,
                keyword,
                symbol,
                ty,
                equal,
                value,
            },
        )
    }

    pub fn add_assignment(
        &mut self,
        symbol: Spanned<Symbol>,
        equal: Span,
        value: Expr,
    ) -> StmtExpr {
        self.push_stmt_expr(
            symbol.span - value.span,
            StmtExprKind::Assignment {
                symbol,
                equal,
                value,
            },
        )
    }

    pub fn add_unreachable(&mut self, span: Span) -> StmtExpr {
        self.push_stmt_expr(span, StmtExprKind::Unreachable)
    }

    pub fn add_continue(&mut self, keyword: Span, label: Option<Label>) -> StmtExpr {
        self.push_stmt_expr(
            label.clone().map_or(keyword, |l| keyword - l.label.span),
            StmtExprKind::Continue { keyword, label },
        )
    }

    pub fn add_break(
        &mut self,
        keyword: Span,
        label: Option<Label>,
        value: Option<Expr>,
    ) -> StmtExpr {
        self.push_stmt_expr(
            value.clone().map_or(
                label.clone().map_or(keyword, |l| keyword - l.label.span),
                |v| keyword - v.span,
            ),
            StmtExprKind::Break {
                keyword,
                label,
                value,
            },
        )
    }

    pub fn add_return(&mut self, keyword: Span, value: Option<Expr>) -> StmtExpr {
        self.push_stmt_expr(
            value.clone().map_or(keyword, |v| keyword - v.span),
            StmtExprKind::Return { keyword, value },
        )
    }

    pub fn add_literal(&mut self, span: Span, literal: Literal) -> Expr {
        self.push_expr(span, ExprKind::Literal(literal))
    }

    pub fn add_unary(&mut self, op_span: Span, op: UnaryOp, value: Expr) -> Expr {
        self.push_expr(
            op_span - value.span,
            ExprKind::Unary {
                op: Spanned {
                    span: op_span,
                    val: op,
                },
                value,
            },
        )
    }

    pub fn add_binary(&mut self, op_span: Span, op: BinaryOp, lhs: Expr, rhs: Expr) -> Expr {
        self.push_expr(
            lhs.span - rhs.span,
            ExprKind::Binary {
                op: Spanned {
                    span: op_span,
                    val: op,
                },
                lhs,
                rhs,
            },
        )
    }

    pub fn add_ident(&mut self, symbol: Spanned<Symbol>) -> Expr {
        self.push_expr(symbol.span, ExprKind::Ident(symbol.val))
    }

    pub fn add_quote(&mut self, span: Span, quote: String) -> Expr {
        self.push_expr(span, ExprKind::Quote(quote))
    }

    pub fn add_bool(&mut self, span: Span, boolean: bool) -> Expr {
        self.push_expr(span, ExprKind::Boolean(boolean))
    }

    pub fn add_type(&mut self, span: Span, builtin_type: BuiltinType) -> Expr {
        self.push_expr(span, ExprKind::BuiltinType(builtin_type))
    }

    pub fn add_unit(&mut self, span: Span) -> Expr {
        self.push_expr(span, ExprKind::Unit)
    }

    pub fn add_block(&mut self, span: Span, statements: NonEmpty<Stmt>) -> Expr {
        self.push_expr(span, ExprKind::Block { statements })
    }

    pub fn add_if(
        &mut self,
        keyword: Span,
        condition: Expr,
        when_body: StmtExpr,
        else_body: Option<(Span, StmtExpr)>,
    ) -> Expr {
        self.push_expr(
            else_body
                .clone()
                .map_or(keyword - when_body.span, |e| keyword - e.1.span),
            ExprKind::If {
                keyword,
                condition,
                when_body,
                else_body,
            },
        )
    }

    pub fn add_loop(&mut self, keyword: Span, body: StmtExpr) -> Expr {
        self.push_expr(keyword - body.span, ExprKind::Loop { keyword, body })
    }

    pub fn add_label(&mut self, label: Label, body: StmtExpr) -> Expr {
        self.push_expr(
            label.label.span - body.span,
            ExprKind::Label { label, body },
        )
    }

    pub fn add_function(
        &mut self,
        keyword: Span,
        parameters: HashMap<Spanned<Symbol>, Expr>,
        output: Expr,
        body: StmtExpr,
    ) -> Expr {
        self.push_expr(
            keyword - body.span,
            ExprKind::Function {
                keyword,
                parameters,
                output,
                body,
            },
        )
    }
}
