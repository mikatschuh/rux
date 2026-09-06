use crate::{
    error::{ErrorCode, Errors},
    grapher::{
        Graph,
        graph::{Data, Type},
    },
    tokenizing::span::Span,
};

pub fn require_type(graph: &Graph, span: Span, ty: Type, value: Data, errors: &mut Errors) -> Data {
    if graph[&value].ty == ty {
        value
    } else {
        errors.push(span, ErrorCode::WrongType);
        graph.err()
    }
}
