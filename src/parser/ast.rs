use std::{collections::HashMap, vec};

use nonempty::NonEmpty;

use crate::{
    literal_parsing::Literal,
    parser::intern::Symbol,
    tokenizing::{
        binary_op::BinaryOp,
        span::{Position, Span},
        token::FloatPrecision,
        unary_op::UnaryOp,
    },
    type_parsing::{IntegerType, TypeSize},
};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub val: T,
}
pub type Ident = Spanned<Symbol>;

#[derive(Clone, Debug)]
pub enum Item {
    Constant {
        keyword: Span,
        ident: Span,
        definition: Definition,
    },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ScopeStmt(usize);

#[derive(Clone, Debug)]
pub enum ScopeStmtKind {
    DeclStmt(DeclStmt),
    Binding {
        keyword: Span,
        mutable: bool,
        ident: Ident,
        definition: Definition,
    },
    BindingWithoutIdent {
        keyword: Span,
        mutable: bool,
        definition: Definition,
    },
    Defer(JumpStruct),
    StmtExpr(StmtExpr), // expression statement would be a single expression used as a statement
    Err,                // An incomplete unparseable statement
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct DeclStmt(usize);

#[derive(Clone, Debug)]
pub enum DeclStmtKind {
    Struct {},
    Enum {},
}

#[derive(Clone, Debug)]
pub enum Definition {
    Type(Expr),
    Assignment {
        ty: Option<Expr>,
        assignment: Assignment,
    },
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub equal: Span,
    pub value: Expr,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct StmtExpr(usize);

#[derive(Clone, Debug)]
pub enum StmtExprKind {
    Assignment {
        ident: Ident,
        assignment: Assignment,
    },
    Unreachable,
    Continue(JumpStruct),
    Break(JumpStruct),
    Return(JumpStruct),
    Expr(Expr),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Expr(usize);

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
        stmts: NonEmpty<ScopeStmt>,
    },

    If {
        keyword: Span,
        condition: Expr,
        when_body: StmtExpr,
        else_body: Option<ControlStruct>,
    },
    Loop(ControlStruct),
    Label {
        label: Label,
        body: StmtExpr,
    },

    Function {
        keyword: Span,
        parameters: HashMap<Ident, Expr>,
        output: Expr,
        body: StmtExpr,
    },

    Err,
}

#[derive(Clone, Debug)]
pub struct Label {
    pub at_sign: Span,
    pub ident: Ident,
}

#[derive(Clone, Debug)]
pub struct ControlStruct {
    pub keyword: Span,
    pub body: StmtExpr,
}

#[derive(Clone, Debug)]
pub struct JumpStruct {
    pub keyword: Span,
    pub label: Option<Label>,
    pub value: Option<Expr>,
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

/// This one is DAG
#[derive(Debug)]
pub struct AstBuilder {
    scope_stmts: Vec<Spanned<ScopeStmtKind>>,
    decl_stmts: Vec<Spanned<DeclStmtKind>>,
    stmt_exprs: Vec<Spanned<StmtExprKind>>,
    exprs: Vec<Spanned<ExprKind>>,
}

impl AstBuilder {
    pub fn new() -> Self {
        Self {
            scope_stmts: vec![],
            decl_stmts: vec![],
            stmt_exprs: vec![],
            exprs: vec![],
        }
    }

    pub fn scope_stmt(&self, scope_stmt: ScopeStmt) -> &Spanned<ScopeStmtKind> {
        &self.scope_stmts[scope_stmt.0]
    }
    pub fn stmt_expr(&self, stmt_expr: StmtExpr) -> &Spanned<StmtExprKind> {
        &self.stmt_exprs[stmt_expr.0]
    }
    pub fn expr(&self, expr: Expr) -> &Spanned<ExprKind> {
        &self.exprs[expr.0]
    }

    pub fn update_start(&mut self, expr: Expr, start: Position) {
        self.exprs[expr.0].span.start = start
    }
    pub fn update_end(&mut self, expr: Expr, end: Position) {
        self.exprs[expr.0].span.end = end
    }

    fn jump_span(&self, jump: &JumpStruct) -> Span {
        let end = match jump.value {
            Some(expr) => self.expr(expr).span.end,
            None => match &jump.label {
                Some(label) => label.ident.span.end,
                None => jump.keyword.end,
            },
        };

        jump.keyword.start - end
    }

    fn add_scope_stmt(&mut self, span: Span, kind: ScopeStmtKind) -> ScopeStmt {
        let id = self.scope_stmts.len();
        self.scope_stmts.push(Spanned { span, val: kind });
        ScopeStmt(id)
    }

    fn add_decl_stmt(&mut self, span: Span, kind: DeclStmtKind) -> DeclStmt {
        let id = self.decl_stmts.len();
        self.decl_stmts.push(Spanned { span, val: kind });
        DeclStmt(id)
    }

    fn add_stmt_expr(&mut self, span: Span, kind: StmtExprKind) -> StmtExpr {
        let id = self.stmt_exprs.len();
        self.stmt_exprs.push(Spanned { span, val: kind });
        StmtExpr(id)
    }

    fn add_expr(&mut self, span: Span, kind: ExprKind) -> Expr {
        let id = self.exprs.len();
        self.exprs.push(Spanned { span, val: kind });
        Expr(id)
    }

    pub fn expr_as_stmt_expr(&mut self, expr: Expr) -> StmtExpr {
        let span = self.expr(expr).span;
        self.add_stmt_expr(span, StmtExprKind::Expr(expr))
    }

    pub fn stmt_expr_as_scope_stmt(&mut self, stmt_expr: StmtExpr) -> ScopeStmt {
        let span = self.stmt_expr(stmt_expr).span;
        self.add_scope_stmt(span, ScopeStmtKind::StmtExpr(stmt_expr))
    }

    pub fn decl_stmt_as_scope_stmt(&mut self, decl_stmt: DeclStmt) -> ScopeStmt {
        let span = self.decl_stmts[decl_stmt.0].span;
        self.add_scope_stmt(span, ScopeStmtKind::DeclStmt(decl_stmt))
    }

    pub fn add_err_expr(&mut self, span: Span) -> Expr {
        self.add_expr(span, ExprKind::Err)
    }

    pub fn add_binding(
        &mut self,
        mutable: bool,
        keyword: Span,
        ident: Ident,
        definition: Definition,
    ) -> ScopeStmt {
        let end = match &definition {
            Definition::Type(ty) => self.expr(*ty).span.end,
            Definition::Assignment { assignment, .. } => self.expr(assignment.value).span.end,
        };

        self.add_scope_stmt(
            keyword - end,
            ScopeStmtKind::Binding {
                mutable,
                keyword,
                ident,
                definition,
            },
        )
    }

    pub fn add_incomplete_binding(
        &mut self,
        mutable: bool,
        keyword: Span,
        definition: Definition,
    ) -> ScopeStmt {
        let end = match &definition {
            Definition::Type(ty) => self.expr(*ty).span.end,
            Definition::Assignment { assignment, .. } => self.expr(assignment.value).span.end,
        };

        self.add_scope_stmt(
            keyword - end,
            ScopeStmtKind::BindingWithoutIdent {
                keyword,
                mutable,
                definition,
            },
        )
    }

    pub fn add_assignment(&mut self, ident: Ident, equal: Span, value: Expr) -> StmtExpr {
        self.add_stmt_expr(
            ident.span - self.expr(value).span,
            StmtExprKind::Assignment {
                ident,
                assignment: Assignment { equal, value },
            },
        )
    }

    pub fn add_unreachable(&mut self, span: Span) -> StmtExpr {
        self.add_stmt_expr(span, StmtExprKind::Unreachable)
    }

    pub fn add_continue(&mut self, jump: JumpStruct) -> StmtExpr {
        self.add_stmt_expr(self.jump_span(&jump), StmtExprKind::Continue(jump))
    }

    pub fn add_break(&mut self, jump: JumpStruct) -> StmtExpr {
        self.add_stmt_expr(self.jump_span(&jump), StmtExprKind::Break(jump))
    }

    pub fn add_return(&mut self, jump: JumpStruct) -> StmtExpr {
        self.add_stmt_expr(self.jump_span(&jump), StmtExprKind::Return(jump))
    }

    pub fn add_literal(&mut self, span: Span, literal: Literal) -> Expr {
        self.add_expr(span, ExprKind::Literal(literal))
    }

    pub fn add_unary(&mut self, op_span: Span, op: UnaryOp, value: Expr) -> Expr {
        self.add_expr(
            op_span - self.expr(value).span,
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
        self.add_expr(
            self.expr(lhs).span - self.expr(rhs).span,
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

    pub fn add_ident(&mut self, ident: Ident) -> Expr {
        self.add_expr(ident.span, ExprKind::Ident(ident.val))
    }

    pub fn add_quote(&mut self, span: Span, quote: String) -> Expr {
        self.add_expr(span, ExprKind::Quote(quote))
    }

    pub fn add_boolean(&mut self, span: Span, boolean: bool) -> Expr {
        self.add_expr(span, ExprKind::Boolean(boolean))
    }

    pub fn add_type(&mut self, span: Span, builtin_type: BuiltinType) -> Expr {
        self.add_expr(span, ExprKind::BuiltinType(builtin_type))
    }

    pub fn add_unit(&mut self, span: Span) -> Expr {
        self.add_expr(span, ExprKind::Unit)
    }

    pub fn add_block(&mut self, span: Span, stmts: NonEmpty<ScopeStmt>) -> Expr {
        self.add_expr(span, ExprKind::Block { stmts })
    }

    pub fn add_if(
        &mut self,
        keyword: Span,
        condition: Expr,
        when_body: StmtExpr,
        else_body: Option<ControlStruct>,
    ) -> Expr {
        let end = match else_body {
            Some(ControlStruct { body, .. }) => self.stmt_expr(body).span.end,
            None => self.stmt_expr(when_body).span.end,
        };

        self.add_expr(
            keyword - end,
            ExprKind::If {
                keyword,
                condition,
                when_body,
                else_body,
            },
        )
    }

    pub fn add_loop(&mut self, keyword: Span, body: StmtExpr) -> Expr {
        self.add_expr(
            keyword - self.stmt_expr(body).span,
            ExprKind::Loop(ControlStruct { keyword, body }),
        )
    }

    pub fn add_label(&mut self, label: Label, body: StmtExpr) -> Expr {
        self.add_expr(
            label.ident.span - self.stmt_expr(body).span,
            ExprKind::Label { label, body },
        )
    }

    pub fn add_function(
        &mut self,
        keyword: Span,
        parameters: HashMap<Ident, Expr>,
        output: Expr,
        body: StmtExpr,
    ) -> Expr {
        self.add_expr(
            keyword - self.stmt_expr(body).span,
            ExprKind::Function {
                keyword,
                parameters,
                output,
                body,
            },
        )
    }
}
