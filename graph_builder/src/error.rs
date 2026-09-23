use tokenizer::Symbol;

use crate::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    MissingEntryPoint { entry: Symbol },

    // variable
    BindingOutsideScope,
    UnknownIdent { symbol: Symbol },
    AssignmentToUnknownIdent { symbol: Symbol },
    AssignmentToImmutableIdent { symbol: Symbol },
    ReadUnitializedOrMoved,

    // control flow
    LabelOverwrite { label: Symbol },
    ContinueOutsideLoop,
    ContinueWithUnknownLabel { label: Symbol },
    BreakOutsideLoop,
    BreakWithUnknownLabel { label: Symbol },
    DivergentControlFlow,

    BindingWithNeitherTypeNorValue,

    // initialization / ownership
    MovedInLoop,

    // type checking
    ExpectedType,
    WrongType { expected: Type, got: Type },
}
