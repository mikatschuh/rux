use tokenizer::Span;

use crate::{
    Diagnostics, Error, Graph,
    graph::{Data, Type},
};

pub fn require_type(
    graph: &Graph,
    span: Span,
    ty: Type,
    value: Data,
    errors: &mut impl Diagnostics,
) -> Data {
    if graph[&value].ty == ty {
        value
    } else {
        errors.add(
            span,
            Error::WrongType {
                expected: ty,
                got: graph[&value].ty.clone(),
            },
        );
        graph.err()
    }
}
