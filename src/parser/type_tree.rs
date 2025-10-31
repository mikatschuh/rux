use std::ops::{Deref, DerefMut};

use bumpalo::boxed::Box as BumpBox;

use crate::{
    error::Span,
    parser::{
        intern::Symbol,
        tokenizing::num::Literal,
        tree::{NodeBox, Scope},
        vars::{VarList, Variable},
    },
};

#[derive(Debug, PartialEq, Eq)]
pub struct TypeWrapper<'src> {
    pub ty: Option<Type<'src>>,
    pub span: Span,
}

impl<'src> TypeWrapper<'src> {
    #[inline]
    pub fn new(span: Span) -> Self {
        Self { span, ty: None }
    }

    #[inline]
    pub fn with_type(mut self, ty: Type<'src>) -> Self {
        self.ty = Some(ty);
        self
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type<'src> {
    Loop {
        condition: NodeBox<'src>,
        then_body: TypeBox<'src>,
        else_body: Option<TypeBox<'src>>,
    },
    If {
        condition: NodeBox<'src>,
        then_body: TypeBox<'src>,
        else_body: Option<TypeBox<'src>>,
    },
    Scope {
        var_table: VarList<'src>,
        scope: Scope<'src>,
    },

    Struct {
        fields: Vec<TypeWrapper<'src>>,
    },
    Unit,

    Id {
        sym: Symbol<'src>,
        lit: Literal,
    },
    Var(Variable<'src>),
    Placeholder,

    Array {
        len: NodeBox<'src>,
        val: TypeBox<'src>,
    },
    Ptr(TypeBox<'src>),

    Deref(TypeBox<'src>),
    FieldAccess {
        val: TypeBox<'src>,
        accessor: Symbol<'src>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeBox<'a> {
    ptr: bumpalo::boxed::Box<'a, TypeWrapper<'a>>,
}

impl<'a> TypeBox<'a> {
    #[inline]
    pub fn new(ptr: BumpBox<'a, TypeWrapper<'a>>) -> Self {
        Self { ptr }
    }
}

impl<'src> Deref for TypeBox<'src> {
    type Target = TypeWrapper<'src>;
    fn deref(&self) -> &Self::Target {
        self.ptr.as_ref()
    }
}

impl<'src> DerefMut for TypeBox<'src> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.ptr.as_mut()
    }
}
