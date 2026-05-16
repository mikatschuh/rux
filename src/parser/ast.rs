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

pub type DeclStmt = Spanned<Rc<DeclStmtKind, NoDealloc>>;
pub type ExprStmt = Spanned<Rc<ExprStmtKind, NoDealloc>>;
pub type Expr = Spanned<Rc<ExprKind, NoDealloc>>;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub val: T,
}

#[derive(Clone, Debug)]
pub enum DeclStmtKind {
    /// Either one, type or value
    Binding {
        keyword: Span,
        mutable: bool,
        symbol: Spanned<Symbol>,
        ty: Option<Expr>,
        assignment: Option<(Span, Expr)>, // equal-sign expr
    },
    ExprStmt {
        expr_stmt: ExprStmt,
    },
}

#[derive(Clone, Debug)]
pub struct Label {
    pub colon: Span,
    pub label: Spanned<Symbol>,
}

#[derive(Clone, Debug)]
pub enum ExprStmtKind {
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
    Expr {
        expr: Expr,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BuiltinType {
    Unit,
    Never,

    Bool,
    Unsigned { size: TypeSize },
    Signed { size: TypeSize },
    Float { precision: FloatPrecision },
    Complit,
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
        statements: NonEmpty<DeclStmt>,
    },

    If {
        keyword: Span,
        condition: Expr,
        when_body: ExprStmt,
        else_body: Option<(Span, ExprStmt)>,
    },
    Loop {
        keyword: Span,
        body: ExprStmt,
    },
    Label {
        label: Label,
        body: ExprStmt,
    },

    Function {
        keyword: Span,
        parameters: HashMap<Spanned<Symbol>, Expr>,
        output: Expr,
        body: ExprStmt,
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

    pub fn arena(self) -> Bump {
        self.arena
    }

    fn push_stmt(&mut self, span: Span, kind: DeclStmtKind) -> DeclStmt {
        Spanned {
            span,
            val: Rc::<DeclStmtKind, NoDealloc>::new_in_bump(kind, &self.arena),
        }
    }
    fn push_expr_stmt(&mut self, span: Span, kind: ExprStmtKind) -> ExprStmt {
        Spanned {
            span,
            val: Rc::<ExprStmtKind, NoDealloc>::new_in_bump(kind, &self.arena),
        }
    }

    fn push_expr(&mut self, span: Span, kind: ExprKind) -> Expr {
        Spanned {
            span,
            val: Rc::<ExprKind, NoDealloc>::new_in_bump(kind, &self.arena),
        }
    }

    pub fn expr_stmt_as_decl_stmt(&mut self, expr_stmt: ExprStmt) -> DeclStmt {
        self.push_stmt(expr_stmt.span, DeclStmtKind::ExprStmt { expr_stmt })
    }

    pub fn expr_as_stmt(&mut self, expr: Expr) -> DeclStmt {
        let expr_stmt = self.expr_as_expr_stmt(expr);
        self.expr_stmt_as_decl_stmt(expr_stmt)
    }

    pub fn expr_as_expr_stmt(&mut self, expr: Expr) -> ExprStmt {
        self.push_expr_stmt(expr.span, ExprStmtKind::Expr { expr })
    }

    pub fn add_binding(
        &mut self,
        mutable: bool,
        keyword: Span,
        symbol: Spanned<Symbol>,
        ty: Option<Expr>,
        assignment: Option<(Span, Expr)>,
    ) -> DeclStmt {
        self.push_stmt(
            keyword
                - assignment
                    .clone()
                    .map_or(ty.clone().map_or(symbol.span, |ty| ty.span), |a| a.1.span),
            DeclStmtKind::Binding {
                mutable,
                keyword,
                symbol,
                ty,
                assignment,
            },
        )
    }

    pub fn add_assignment(
        &mut self,
        symbol: Spanned<Symbol>,
        equal: Span,
        value: Expr,
    ) -> ExprStmt {
        self.push_expr_stmt(
            symbol.span - value.span,
            ExprStmtKind::Assignment {
                symbol,
                equal,
                value,
            },
        )
    }

    pub fn add_unreachable(&mut self, span: Span) -> ExprStmt {
        self.push_expr_stmt(span, ExprStmtKind::Unreachable)
    }

    pub fn add_continue(&mut self, keyword: Span, label: Option<Label>) -> ExprStmt {
        self.push_expr_stmt(
            label.clone().map_or(keyword, |l| keyword - l.label.span),
            ExprStmtKind::Continue { keyword, label },
        )
    }

    pub fn add_break(
        &mut self,
        keyword: Span,
        label: Option<Label>,
        value: Option<Expr>,
    ) -> ExprStmt {
        self.push_expr_stmt(
            value.clone().map_or(
                label.clone().map_or(keyword, |l| keyword - l.label.span),
                |v| keyword - v.span,
            ),
            ExprStmtKind::Break {
                keyword,
                label,
                value,
            },
        )
    }

    pub fn add_return(&mut self, keyword: Span, value: Option<Expr>) -> ExprStmt {
        self.push_expr_stmt(
            value.clone().map_or(keyword, |v| keyword - v.span),
            ExprStmtKind::Return { keyword, value },
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

    pub fn add_boolean(&mut self, span: Span, boolean: bool) -> Expr {
        self.push_expr(span, ExprKind::Boolean(boolean))
    }

    pub fn add_type(&mut self, span: Span, builtin_type: BuiltinType) -> Expr {
        self.push_expr(span, ExprKind::BuiltinType(builtin_type))
    }

    pub fn add_unit(&mut self, span: Span) -> Expr {
        self.push_expr(span, ExprKind::Unit)
    }

    pub fn add_block(&mut self, span: Span, statements: NonEmpty<DeclStmt>) -> Expr {
        self.push_expr(span, ExprKind::Block { statements })
    }

    pub fn add_if(
        &mut self,
        keyword: Span,
        condition: Expr,
        when_body: ExprStmt,
        else_body: Option<(Span, ExprStmt)>,
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

    pub fn add_loop(&mut self, keyword: Span, body: ExprStmt) -> Expr {
        self.push_expr(keyword - body.span, ExprKind::Loop { keyword, body })
    }

    pub fn add_label(&mut self, label: Label, body: ExprStmt) -> Expr {
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
        body: ExprStmt,
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
