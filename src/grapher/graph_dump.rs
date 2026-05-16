use std::collections::HashMap;

use petgraph::{graph::NodeIndex, visit::EdgeRef};

use crate::{
    grapher::{
        builder::DataCursor,
        graph::{
            BranchID, CtrlID, CtrlKind, DataID, DataKind, MergeID, TypeID, TypeKind, UniqueNodes,
        },
    },
    parser::Interner,
    tokenizing,
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

type Visited = HashMap<usize, NodeIndex>;

pub fn dump_text(UniqueNodes { data, types }: UniqueNodes, cursor: Option<DataCursor>) -> String {
    let mut visited = Visited::new();

    let mut graph_dump: GraphDump = petgraph::Graph::new();

    /*for (symbol, mutable, ty, value) in bindings {
        let name = graph_dump.add_node(interner.resolve(symbol).to_string());
        let value = match value {
            Some(value) => process_data_node(&mut graph_dump, &mut visited, value),
            None => graph_dump.add_node("uninitialized".to_string()),
        };
        graph_dump.add_edge(
            name,
            value,
            if mutable {
                "mutable".to_string()
            } else {
                "immutable".to_string()
            },
        );
    }*/

    for node in data {
        process_data_node(&mut graph_dump, &mut visited, node);
    }

    for node in types {
        process_type_node(&mut graph_dump, &mut visited, node);
    }

    if let Some(DataCursor { ctrl, data, .. }) = cursor {
        process_data_node(&mut graph_dump, &mut visited, data);

        let current = process_ctrl_node(&mut graph_dump, &mut visited, ctrl);

        let current_node = graph_dump.add_node(mem!("current"));
        graph_dump.add_edge(current_node, current, "".to_string());
    }

    dump_cytoscape(&graph_dump)
}

pub fn process_data_node(graph: &mut GraphDump, visited: &mut Visited, node: DataID) -> NodeIndex {
    let node_addr = node.addr();
    if let Some(idx) = visited.get(&node_addr) {
        return *idx;
    }
    use DataKind::*;
    let ty = process_type_node(graph, visited, node.ty.clone());
    let data = match node.kind.clone() {
        Literal { literal } => {
            let idx = graph.add_node(format!("lit {}", literal));
            visited.insert(node_addr, idx);
            idx
        }
        Quote { quote } => {
            let idx = graph.add_node(format!(
                "lit \"{}\"",
                tokenizing::with_written_out_escape_sequences(&quote)
            ));
            visited.insert(node_addr, idx);
            idx
        }
        Boolean(boolean) => {
            let idx = graph.add_node(format!("lit {}", boolean));
            visited.insert(node_addr, idx);
            idx
        }
        Unit => {
            let idx = graph.add_node("lit unit".to_string());
            visited.insert(node_addr, idx);
            idx
        }
        Never => {
            let idx = graph.add_node("lit never".to_string());
            visited.insert(node_addr, idx);
            idx
        }

        Unary { op, value } => {
            let op = graph.add_node(format!("operator {}", op));
            visited.insert(node_addr, op);
            let input = process_data_node(graph, visited, value);
            graph.add_edge(op, input, "".to_string());
            op
        }
        Binary { op, lhs, rhs } => {
            let op = graph.add_node(format!("operator {}", op));
            visited.insert(node_addr, op);
            let lhs = process_data_node(graph, visited, lhs);
            let rhs = process_data_node(graph, visited, rhs);
            graph.add_edge(op, lhs, "a".to_string());
            graph.add_edge(op, rhs, "b".to_string());
            op
        }
        Load { .. } => todo!(),

        Phi { phi } => {
            let phi_node = graph.add_node("phi".to_string());
            visited.insert(node_addr, phi_node);
            let merge = process_merge_node(graph, visited, phi.merge.clone());

            phi.variants.iter().enumerate().for_each(|(i, v)| {
                let variant = process_data_node(graph, visited, v.clone());
                graph.add_edge(phi_node, variant, format!("{}", i));
            });
            graph.add_edge(phi_node, merge, mem!("ctrl"));
            phi_node
        }

        Type { ty } => {
            let idx = process_type_node(graph, visited, ty);
            visited.insert(node_addr, idx);
            idx
        }

        Error => {
            let idx = graph.add_node("error".to_string());
            visited.insert(node_addr, idx);
            idx
        }
    };
    graph.add_edge(data, ty, ty!("type"));
    data
}

pub fn process_type_node(graph: &mut GraphDump, visited: &mut Visited, node: TypeID) -> NodeIndex {
    let node_addr = node.addr();
    if let Some(idx) = visited.get(&node_addr) {
        return *idx;
    }
    use TypeKind::*;
    match &*node {
        Type => {
            let idx = graph.add_node(ty!("type"));
            visited.insert(node_addr, idx);
            idx
        }
        BuiltinType(builtin_type) => {
            let idx = graph.add_node(ty!("builtin_type[{:?}]", builtin_type));
            visited.insert(node_addr, idx);
            idx
        }

        DataType { data } => process_data_node(graph, visited, data.clone()),
        Error => {
            let idx = graph.add_node(ty!("error"));
            visited.insert(node_addr, idx);
            idx
        }
    }
}

pub fn process_merge_node(
    graph: &mut GraphDump,
    visited: &mut Visited,
    node: MergeID,
) -> NodeIndex {
    let node_addr = node.addr();
    if let Some(idx) = visited.get(&node_addr) {
        return *idx;
    }

    let merge = graph.add_node(mem!(
        "{}",
        if node.branches.is_empty() {
            "never"
        } else {
            "merge"
        }
    ));
    visited.insert(node_addr, merge);
    node.branches.iter().enumerate().for_each(|(i, b)| {
        let branch = process_ctrl_node(graph, visited, b.clone());
        graph.add_edge(merge, branch, mem!("{}", i));
    });
    merge
}

pub fn process_branch_node(
    graph: &mut GraphDump,
    visited: &mut Visited,
    node: BranchID,
) -> NodeIndex {
    let node_addr = node.addr();
    if let Some(idx) = visited.get(&node_addr) {
        return *idx;
    }

    let branch = graph.add_node(mem!("branch"));
    visited.insert(node_addr, branch);
    let ctrl = process_ctrl_node(graph, visited, node.ctrl.clone());
    let condition = process_data_node(graph, visited, node.condition.clone());
    graph.add_edge(branch, ctrl, mem!("ctrl"));
    graph.add_edge(branch, condition, "condition".to_string());
    branch
}

pub fn process_ctrl_node(graph: &mut GraphDump, visited: &mut Visited, node: CtrlID) -> NodeIndex {
    let node_addr = node.addr();
    if let Some(idx) = visited.get(&node_addr) {
        return *idx;
    }

    use CtrlKind::*;
    match (*node).clone() {
        Start => {
            let idx = graph.add_node(mem!("start"));
            visited.insert(node_addr, idx);
            idx
        }
        FalseBranch { branch } => {
            let false_branch = graph.add_node(mem!("false branch"));
            visited.insert(node_addr, false_branch);
            let branch = process_branch_node(graph, visited, branch.clone());
            graph.add_edge(false_branch, branch, mem!("branch"));
            false_branch
        }
        TrueBranch { branch } => {
            let true_branch = graph.add_node(mem!("true branch"));
            visited.insert(node_addr, true_branch);
            let branch = process_branch_node(graph, visited, branch.clone());
            graph.add_edge(true_branch, branch, mem!("branch"));
            true_branch
        }
        Merge { merge } => {
            let idx = process_merge_node(graph, visited, merge);
            visited.insert(node_addr, idx);
            idx
        }
    }
}

fn dump_cytoscape(g: &GraphDump) -> String {
    let elements = build_elements(g);
    let template = include_str!("graph_template.html");
    template.replace("__ELEMENTS__", &elements)
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
