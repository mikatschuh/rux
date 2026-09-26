use std::collections::HashMap;

use petgraph::{graph::NodeIndex, visit::EdgeRef};

use tokenizer::{Interner, Symbol};

use crate::{
    builder::DataCursor,
    graph::{Branch, Ctrl, CtrlKind, Data, DataKind, Graph, Merge, Type, TypeKind},
};

macro_rules! mem {
    ($($arg:tt)*) => {
        format!("mem {}", format!($($arg)*))
    };
    () => {

    }
}

macro_rules! ty {
    ($($arg:tt)*) => {
        format!("type {}", format!($($arg)*))
    };
    () => {

    }
}

type GraphDump = petgraph::Graph<String, String>;

#[derive(Clone, PartialEq, Eq, Hash)]
enum NodeID {
    Data(Data),
    Type(Type),
    Merge(Merge),
    Branch(Branch),
    Ctrl(Ctrl),
}

type Visited = HashMap<NodeID, NodeIndex>;

pub fn dump_text(
    source: &Graph<'_>,
    symbols: Vec<(Symbol, Data)>,
    cursor: Option<DataCursor>,
    interner: &Interner<'_>,
) -> String {
    let mut visited = Visited::new();

    let mut graph_dump: GraphDump = petgraph::Graph::new();

    for node in source.type_ids() {
        process_type_node(source, &mut graph_dump, &mut visited, node);
    }

    for (symbol, value) in symbols {
        let value = process_data_node(source, &mut graph_dump, &mut visited, value);

        let name = graph_dump.add_node(interner.resolve(symbol).to_string());
        graph_dump.add_edge(name, value, "".to_string());
    }

    if let Some(DataCursor { ctrl, data, .. }) = cursor {
        process_data_node(source, &mut graph_dump, &mut visited, data);

        let current = process_ctrl_node(source, &mut graph_dump, &mut visited, ctrl);

        let current_node = graph_dump.add_node(mem!("current"));
        graph_dump.add_edge(current_node, current, "".to_string());
    }

    dump_cytoscape(&graph_dump)
}

fn process_data_node(
    source: &Graph<'_>,
    graph: &mut GraphDump,
    visited: &mut Visited,
    node: Data,
) -> NodeIndex {
    let node_id = NodeID::Data(node);
    if let Some(idx) = visited.get(&node_id) {
        return *idx;
    }
    use DataKind::*;
    // let ty = process_type_node(source, graph, visited, source[node].ty.clone());
    let data = match source[node].clone() {
        Literal(literal) => {
            let idx = graph.add_node(format!("lit {}", literal));
            visited.insert(node_id, idx);
            idx
        }
        Quote(quote) => {
            let idx = graph.add_node(format!(
                "lit \"{}\"",
                tokenizer::with_written_out_escape_sequences(&quote)
            ));
            visited.insert(node_id, idx);
            idx
        }
        Boolean(boolean) => {
            let idx = graph.add_node(format!("lit {}", boolean));
            visited.insert(node_id, idx);
            idx
        }
        Unit => {
            let idx = graph.add_node("lit unit".to_string());
            visited.insert(node_id, idx);
            idx
        }

        Unary { op, value } => {
            let op = graph.add_node(format!("operator {}", op));
            visited.insert(node_id, op);
            let input = process_data_node(source, graph, visited, value);
            graph.add_edge(op, input, "".to_string());
            op
        }
        Binary {
            op,
            ops: [lhs, rhs],
        } => {
            let op = graph.add_node(format!("operator {}", op));
            visited.insert(node_id, op);
            let lhs = process_data_node(source, graph, visited, lhs);
            let rhs = process_data_node(source, graph, visited, rhs);
            graph.add_edge(op, lhs, "a".to_string());
            graph.add_edge(op, rhs, "b".to_string());
            op
        }
        Load { .. } => todo!(),

        Phi { merge, variants } => {
            let phi_node = graph.add_node("phi".to_string());
            visited.insert(node_id, phi_node);
            let merge = process_merge_node(source, graph, visited, merge);

            variants.iter().enumerate().for_each(|(i, v)| {
                let variant = process_data_node(source, graph, visited, *v);
                graph.add_edge(phi_node, variant, format!("{}", i));
            });
            graph.add_edge(phi_node, merge, mem!("ctrl"));
            phi_node
        }

        Type { ty } => {
            let idx = process_type_node(source, graph, visited, ty);
            visited.insert(node_id, idx);
            idx
        }

        Err => {
            let idx = graph.add_node("error".to_string());
            visited.insert(node_id, idx);
            idx
        }
        Placeholder => {
            let idx = graph.add_node("error".to_string());
            visited.insert(node_id, idx);
            idx
        }
    };
    // graph.add_edge(data, ty, ty!("type"));
    data
}

fn process_type_node(
    source: &Graph<'_>,
    graph: &mut GraphDump,
    visited: &mut Visited,
    node: Type,
) -> NodeIndex {
    let node_id = NodeID::Type(node);
    if let Some(idx) = visited.get(&node_id) {
        return *idx;
    }
    use TypeKind::*;
    match &source[node] {
        Type => {
            let idx = graph.add_node(ty!("type"));
            visited.insert(node_id, idx);
            idx
        }
        BuiltinType(builtin_type) => {
            let idx = graph.add_node(ty!("builtin_type[{:?}]", builtin_type));
            visited.insert(node_id, idx);
            idx
        }

        TypeData { data } => process_data_node(source, graph, visited, *data),
        Err => {
            let idx = graph.add_node(ty!("error"));
            visited.insert(node_id, idx);
            idx
        }
    }
}

fn process_merge_node(
    source: &Graph<'_>,
    graph: &mut GraphDump,
    visited: &mut Visited,
    node: Merge,
) -> NodeIndex {
    let node_id = NodeID::Merge(node);
    if let Some(idx) = visited.get(&node_id) {
        return *idx;
    }

    let merge = graph.add_node(mem!(
        "{}",
        if source[node].prev.is_empty() {
            "never"
        } else {
            "merge"
        }
    ));
    visited.insert(node_id, merge);
    source[node].prev.iter().enumerate().for_each(|(i, b)| {
        let branch = process_ctrl_node(source, graph, visited, *b);
        graph.add_edge(merge, branch, mem!("{}", i));
    });
    merge
}

fn process_branch_node(
    source: &Graph<'_>,
    graph: &mut GraphDump,
    visited: &mut Visited,
    node: Branch,
) -> NodeIndex {
    let node_id = NodeID::Branch(node);
    if let Some(idx) = visited.get(&node_id) {
        return *idx;
    }

    let branch = graph.add_node(mem!("branch"));
    visited.insert(node_id, branch);
    let ctrl = process_ctrl_node(source, graph, visited, source[node].parent);
    let condition = process_data_node(source, graph, visited, source[node].condition);
    graph.add_edge(branch, ctrl, mem!("ctrl"));
    graph.add_edge(branch, condition, "condition".to_string());
    branch
}

fn process_ctrl_node(
    source: &Graph<'_>,
    graph: &mut GraphDump,
    visited: &mut Visited,
    node: Ctrl,
) -> NodeIndex {
    let node_id = NodeID::Ctrl(node);
    if let Some(idx) = visited.get(&node_id) {
        return *idx;
    }

    use CtrlKind::*;
    match source[node].clone() {
        Entry => {
            let idx = graph.add_node(mem!("start"));
            visited.insert(node_id, idx);
            idx
        }
        Branch { branch, idx: 0 } => {
            let false_branch = graph.add_node(mem!("false branch"));
            visited.insert(node_id, false_branch);
            let branch = process_branch_node(source, graph, visited, branch);
            graph.add_edge(false_branch, branch, mem!("branch"));
            false_branch
        }
        Branch { branch, idx: _ } => {
            let true_branch = graph.add_node(mem!("true branch"));
            visited.insert(node_id, true_branch);
            let branch = process_branch_node(source, graph, visited, branch);
            graph.add_edge(true_branch, branch, mem!("branch"));
            true_branch
        }
        Merge { merge } => {
            let idx = process_merge_node(source, graph, visited, merge);
            visited.insert(node_id, idx);
            idx
        }
        Placeholder => {
            let idx = graph.add_node("error".to_string());
            visited.insert(node_id, idx);
            idx
        }
    }
}

fn dump_cytoscape(g: &GraphDump) -> String {
    let elements = build_elements(g);
    let template = include_str!("graph_template.html");
    template
        .replace("__ELEMENTS__", &elements)
        .replace("  ", "")
        .replace("\n", "")
}

fn build_elements(g: &GraphDump) -> String {
    let mut out = String::new();

    for idx in g.node_indices() {
        let label = g[idx].replace('\'', "\\'");
        let (group, label) = if let Some(label) = label.strip_prefix("operator") {
            ("operator", label.trim())
        } else if label == "phi" {
            ("operator", label.as_ref())
        } else if let Some(label) = label.strip_prefix("lit") {
            ("literal", label.trim())
        } else if let Some(label) = label.strip_prefix("mem") {
            ("memory", label.trim())
        } else if let Some(label) = label.strip_prefix("type") {
            ("type", label.trim())
        } else {
            ("variable", label.as_ref())
        };
        out += &format!(
            "{{ data: {{ id: '{}', label: '{}', group: '{}' }} }},\n",
            idx.index(),
            label
                .replace('\'', "\\'")
                .replace('\\', "\\\\")
                .replace('"', "\\\""),
            group
        );
    }

    for edge in g.edge_references() {
        let label = edge
            .weight()
            .to_string()
            .replace('\'', "\\'")
            .replace('\\', "\\\\");
        let (group, label) = if let Some(label) = label.strip_prefix("mem") {
            ("memory", label.trim().to_string())
        } else if let Some(label) = label.strip_prefix("type") {
            ("type", label.trim().to_string())
        } else {
            ("value", label)
        };

        out += &format!(
            "{{ data: {{ source: '{}', target: '{}', label: '{}', group: '{}' }} }},\n",
            edge.source().index(),
            edge.target().index(),
            label,
            group,
        );
    }

    out
}
