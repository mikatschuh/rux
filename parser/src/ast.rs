use std::{collections::HashMap, vec};

use tokenizer::{FloatPrecision, IntegerType, Literal, Position, Span, Symbol, TypeSize};

use crate::{BinaryOp, UnaryOp};

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct Spanned<T> {
    pub span: Span,
    pub val: T,
}
pub type Ident = Spanned<Symbol>;
impl Ident {
    pub fn from_parts(symbol: Symbol, span: Span) -> Self {
        Self { span, val: symbol }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct ScopeStmt(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct DeclStmt(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct TypeDecl(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct StmtExpr(usize);

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Expr(usize);

#[derive(Clone, Debug)]
pub enum Item {
    Constant {
        keyword: Span,
        ident: Span,
        definition: Definition,
    },
    DeclStmt(DeclStmt),
}

#[derive(Clone, Debug)]
pub enum ScopeStmtKind {
    DeclStmt(DeclStmt),
    Binding {
        keyword: Span,
        mutable: bool,
        ident: Ident,
        definition: Definition,
    },
    IncompleteBinding {
        keyword: Span,
    },
    Defer(JumpStruct),
    StmtExpr(StmtExpr), // expression statement would be a single expression used as a statement
}

#[derive(Clone, Debug)]
pub enum DeclStmtKind {
    Function {
        keyword: Span,
        ident: Ident,
        parameters: HashMap<Ident, Expr>,
        output: Expr,
        body: StmtExpr,
    },
    Struct {
        ident: Ident,
    },
    Enum {
        keyword: Span,
        ident: Ident,
        variants: HashMap<Symbol, Parameter>,
    },
    IncompleteDecl {
        keyword: Span,
    },
}

#[derive(Clone, Debug)]
pub struct Parameter {
    ident: Span,
    ty: Option<Expr>,
}

#[derive(Debug)]
pub enum TypeDeclKind {
    Struct {
        keyword: Span,
        fields: HashMap<Symbol, Parameter>,
    },
    Enum {
        keyword: Span,
        variants: HashMap<Symbol, Parameter>,
    },
    Newtype {
        keyword: Span,
        ty: Expr,
    },
}

#[derive(Clone, Debug)]
pub enum Definition {
    Type {
        ty: Expr,
        assignment: Option<Assignment>,
    },
    Assignment(Assignment),
}

#[derive(Clone, Debug)]
pub struct Assignment {
    pub equal: Span,
    pub value: Expr,
}

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

#[derive(Clone, Debug)]
pub enum ExprKind<'src> {
    Ident(Symbol),

    BuiltinType(BuiltinType),
    Literal(Literal<'src>),
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
    FieldAccess {
        lhs: Expr,
        dot_span: Span,
        accessor: Ident,
    }, // Field Projection

    Block {
        stmts: Box<[ScopeStmt]>,
    },

    If {
        keyword: Span,
        condition: Expr,
        when_body: StmtExpr,
        else_clause: Option<ControlStruct>,
    },
    Loop(ControlStruct),
    Label {
        label: Label,
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
pub struct AstBuilder<'src> {
    scope_stmts: Vec<Spanned<ScopeStmtKind>>,
    decl_stmts: Vec<Spanned<DeclStmtKind>>,
    type_decls: Vec<Spanned<TypeDeclKind>>,
    stmt_exprs: Vec<Spanned<StmtExprKind>>,
    exprs: Vec<Spanned<ExprKind<'src>>>,
}

impl<'src> AstBuilder<'src> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            scope_stmts: vec![],
            decl_stmts: vec![],
            type_decls: vec![],
            stmt_exprs: vec![],
            exprs: vec![],
        }
    }

    pub fn update_start(&mut self, expr: &mut Expr, start: Position) {
        self.exprs[expr.0].span.start = start
    }
    pub fn update_end(&mut self, expr: &mut Expr, end: Position) {
        self.exprs[expr.0].span.end = end
    }

    fn jump_span(&self, jump: &JumpStruct) -> Span {
        let end = match &jump.value {
            Some(expr) => self[*expr].span.end,
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

    fn add_expr(&mut self, span: Span, kind: ExprKind<'src>) -> Expr {
        let id = self.exprs.len();
        self.exprs.push(Spanned { span, val: kind });
        Expr(id)
    }

    pub fn expr_as_stmt_expr(&mut self, expr: Expr) -> StmtExpr {
        let span = self[expr].span;
        self.add_stmt_expr(span, StmtExprKind::Expr(expr))
    }

    pub fn stmt_expr_as_scope_stmt(&mut self, stmt_expr: StmtExpr) -> ScopeStmt {
        let span = self[stmt_expr].span;
        self.add_scope_stmt(span, ScopeStmtKind::StmtExpr(stmt_expr))
    }

    pub fn decl_stmt_as_scope_stmt(&mut self, decl_stmt: DeclStmt) -> ScopeStmt {
        let span = self[decl_stmt].span;
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
            Definition::Assignment(assignment)
            | Definition::Type {
                assignment: Some(assignment),
                ..
            } => self[assignment.value].span.end,
            Definition::Type {
                ty,
                assignment: None,
            } => self[*ty].span.end,
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

    pub fn add_incomplete_binding(&mut self, keyword: Span) -> ScopeStmt {
        self.add_scope_stmt(keyword, ScopeStmtKind::IncompleteBinding { keyword })
    }

    pub fn add_assignment(&mut self, ident: Ident, equal: Span, value: Expr) -> StmtExpr {
        self.add_stmt_expr(
            ident.span - self[value].span,
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

    pub fn add_literal(&mut self, span: Span, literal: Literal<'src>) -> Expr {
        self.add_expr(span, ExprKind::Literal(literal))
    }

    pub fn add_unary(&mut self, op_span: Span, op: UnaryOp, value: Expr) -> Expr {
        self.add_expr(
            op_span - self[value].span,
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
            self[lhs].span - self[rhs].span,
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

    pub fn add_field_access(&mut self, dot_span: Span, lhs: Expr, accessor: Ident) -> Expr {
        self.add_expr(
            self[lhs].span - accessor.span,
            ExprKind::FieldAccess {
                lhs,
                dot_span,
                accessor,
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

    pub fn add_block(&mut self, span: Span, stmts: Box<[ScopeStmt]>) -> Expr {
        self.add_expr(span, ExprKind::Block { stmts })
    }

    pub fn add_if(
        &mut self,
        keyword: Span,
        condition: Expr,
        when_body: StmtExpr,
        else_clause: Option<ControlStruct>,
    ) -> Expr {
        let end = match &else_clause {
            Some(ControlStruct { body, .. }) => self[*body].span.end,
            None => self[when_body].span.end,
        };

        self.add_expr(
            keyword - end,
            ExprKind::If {
                keyword,
                condition,
                when_body,
                else_clause,
            },
        )
    }

    pub fn add_loop(&mut self, keyword: Span, body: StmtExpr) -> Expr {
        self.add_expr(
            keyword - self[body].span,
            ExprKind::Loop(ControlStruct { keyword, body }),
        )
    }

    pub fn add_label(&mut self, label: Label, body: StmtExpr) -> Expr {
        self.add_expr(
            label.ident.span - self[body].span,
            ExprKind::Label { label, body },
        )
    }

    pub fn add_function(
        &mut self,
        keyword: Span,
        ident: Ident,
        parameters: HashMap<Ident, Expr>,
        output: Expr,
        body: StmtExpr,
    ) -> DeclStmt {
        self.add_decl_stmt(
            keyword - self[body].span,
            DeclStmtKind::Function {
                keyword,
                ident,
                parameters,
                output,
                body,
            },
        )
    }

    pub fn add_incomplete_decl(&mut self, keyword: Span) -> DeclStmt {
        self.add_decl_stmt(keyword, DeclStmtKind::IncompleteDecl { keyword })
    }
}

mod graph_indexing {
    use std::ops::Index;

    use super::{
        AstBuilder, DeclStmt, DeclStmtKind, Expr, ExprKind, ScopeStmt, ScopeStmtKind, Spanned,
        StmtExpr, StmtExprKind, TypeDecl, TypeDeclKind,
    };

    impl<'src> Index<ScopeStmt> for AstBuilder<'src> {
        type Output = Spanned<ScopeStmtKind>;
        fn index(&self, index: ScopeStmt) -> &Self::Output {
            &self.scope_stmts[index.0]
        }
    }

    impl<'src> Index<DeclStmt> for AstBuilder<'src> {
        type Output = Spanned<DeclStmtKind>;
        fn index(&self, index: DeclStmt) -> &Self::Output {
            &self.decl_stmts[index.0]
        }
    }

    impl<'src> Index<TypeDecl> for AstBuilder<'src> {
        type Output = Spanned<TypeDeclKind>;
        fn index(&self, index: TypeDecl) -> &Self::Output {
            &self.type_decls[index.0]
        }
    }

    impl<'src> Index<StmtExpr> for AstBuilder<'src> {
        type Output = Spanned<StmtExprKind>;
        fn index(&self, index: StmtExpr) -> &Self::Output {
            &self.stmt_exprs[index.0]
        }
    }

    impl<'src> Index<Expr> for AstBuilder<'src> {
        type Output = Spanned<ExprKind<'src>>;
        fn index(&self, index: Expr) -> &Self::Output {
            &self.exprs[index.0]
        }
    }
}
