use crate::{
    error::{ErrorCode, Errors, Span},
    grapher::{
        Graph,
        graph::{DataID, TypeID},
    },
};

pub fn require_type(
    graph: &Graph,
    span: Span,
    ty: TypeID,
    value: DataID,
    errors: &mut Errors,
) -> DataID {
    if value.ty.ptr_cmp(&ty) {
        value
    } else {
        errors.push(span, ErrorCode::WrongType);
        graph.error()
    }
}
