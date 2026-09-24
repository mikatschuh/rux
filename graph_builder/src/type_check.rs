use tokenizer::Span;

use crate::{
    Diagnostics, Error, Graph,
    graph::{Data, Type},
};

pub fn require_type(
    graph: &Graph<'_>,
    span: Span,
    ty: Type,
    value: Data,
    errors: &mut impl Diagnostics,
) -> Data {
    if graph.get_type(value) == ty {
        value
    } else {
        errors.add(
            span,
            Error::WrongType {
                expected: ty,
                got: graph.get_type(value),
            },
        );
        graph.err()
    }
}
