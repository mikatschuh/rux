use crate::{
    comp,
    error::{Error, ErrorCode, Span},
    parser::{
        intern::Symbol,
        tree::{NodeBox, TreeDisplay},
    },
};
use std::{collections::HashMap, fmt, marker::PhantomData};

pub trait LabelTable<'src> {
    const IS_SCOPED: bool;
    fn new_label(
        &mut self,
        span: Span,
        sym: Symbol<'src>,
        ty: NodeBox<'src>,
    ) -> Result<(), Error<'src>>;
    fn label_used(&mut self, sym: Symbol<'src>) -> Option<Variable<'src>>;
    fn open_branch(&mut self);
    fn close_branch(&mut self);
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct Variable<'table> {
    _marker: PhantomData<&'table ()>,
    idx: usize,
}

impl Variable<'_> {
    fn new(idx: usize) -> Self {
        Self {
            _marker: PhantomData::default(),
            idx,
        }
    }
}

impl<'src> fmt::Display for Variable<'src> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "var[ {} ]", self.idx)
    }
}

pub type VarTable<'src> = Vec<NodeBox<'src>>;

impl<'src> TreeDisplay<'src> for VarTable<'src> {
    fn display(
        &self,
        internalizer: &super::intern::Internalizer<'src>,
        indentation: &String,
    ) -> String {
        let mut output = "VarTable".to_owned();
        for (idx, var) in self.iter().enumerate() {
            let new_indentation = format!("{indentation}   ",);
            output += &format!(
                "\n{indentation} * {}\n{}╰─ {}",
                Variable::new(idx),
                new_indentation.clone(),
                var.display(internalizer, &(new_indentation + "   "))
            )
        }
        output
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ScopedSymTable<'src> {
    table: VarTable<'src>,
    scopes: comp::Vec<HashMap<Symbol<'src>, Variable<'src>>, 1>,
}

impl<'src> ScopedSymTable<'src> {
    pub fn new() -> Self {
        Self {
            table: vec![],
            scopes: comp::Vec::new([HashMap::new()]),
        }
    }

    #[inline]
    pub fn output_table(self) -> VarTable<'src> {
        self.table
    }
}

impl<'src> LabelTable<'src> for ScopedSymTable<'src> {
    const IS_SCOPED: bool = true;
    fn new_label(
        &mut self,
        _: Span,
        sym: Symbol<'src>,
        ty: NodeBox<'src>,
    ) -> Result<(), Error<'src>> {
        let var = Variable::new(self.table.len());

        self.table.push(ty);
        self.scopes.last_mut().insert(sym, var);
        Ok(())
    }

    fn label_used(&mut self, sym: Symbol<'src>) -> Option<Variable<'src>> {
        for sym_var_table in self.scopes.iter().rev() {
            if let Some(var) = sym_var_table.get(&sym) {
                return Some(*var);
            }
        }
        None
    }

    fn open_branch(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn close_branch(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        } else {
            panic!("VarTable::close_last_scope was called even though there was no scope to close")
        }
    }
}

#[derive(Debug)]
pub struct ParamTable<'src> {
    parameters: VarTable<'src>, // types
    sym_param_table: HashMap<Symbol<'src>, Variable<'src>>,
}

impl<'src> ParamTable<'src> {
    pub fn new() -> Self {
        Self {
            parameters: vec![],
            sym_param_table: HashMap::new(),
        }
    }

    pub fn output_params(self) -> VarTable<'src> {
        self.parameters
    }

    pub fn make_scoped(self) -> (HashMap<Symbol<'src>, Variable<'src>>, ScopedSymTable<'src>) {
        (
            self.sym_param_table.clone(),
            ScopedSymTable {
                table: self.parameters,
                scopes: comp::Vec::new([self.sym_param_table]),
            },
        )
    }
}

impl<'src> LabelTable<'src> for ParamTable<'src> {
    const IS_SCOPED: bool = false;

    fn new_label(
        &mut self,
        span: Span,
        sym: Symbol<'src>,
        ty: NodeBox<'src>,
    ) -> Result<(), Error<'src>> {
        let new_param = Variable::new(self.parameters.len());
        let span = span - ty.span;
        self.parameters.push(ty);
        match self.sym_param_table.insert(sym, new_param) {
            Some(_) => Err(Error::new(span, ErrorCode::ParamShadowing)),
            None => Ok(()),
        }
    }

    #[inline]
    fn label_used(&mut self, _: Symbol<'src>) -> Option<Variable<'src>> {
        None
    }
    #[inline]
    fn open_branch(&mut self) {}
    #[inline]
    fn close_branch(&mut self) {}
}

#[derive(Debug, PartialEq, Eq)]
pub struct VarTableMoc;

impl<'src> LabelTable<'src> for VarTableMoc {
    const IS_SCOPED: bool = true;
    #[inline]
    fn new_label(&mut self, _: Span, _: Symbol<'src>, _: NodeBox<'src>) -> Result<(), Error<'src>> {
        Ok(())
    }
    #[inline]
    fn label_used(&mut self, _: Symbol<'src>) -> Option<Variable<'src>> {
        None
    }
    #[inline]
    fn open_branch(&mut self) {}
    #[inline]
    fn close_branch(&mut self) {}
}
