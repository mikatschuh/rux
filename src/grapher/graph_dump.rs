use std::collections::HashMap;

use petgraph::{graph::NodeIndex, visit::EdgeRef};

use crate::{
    grapher::{
        Graph,
        graph::{CtrlID, CtrlKind, DataID, DataKind, MergeID},
        parser::{ScopedSymbolTable, SymbolDump},
    },
    tokenizing,
};

macro_rules! mem {
    ($($arg:tt)*) => {
        format!("mem {}", format!($($arg)*))
    };
    () => {

    }
}

type GraphDump = petgraph::Graph<String, String>;

#[derive(Debug, Default)]
pub struct Visited<'src> {
    data: HashMap<DataID<'src>, NodeIndex>,
    ctrl: HashMap<CtrlID<'src>, NodeIndex>,
    merge: HashMap<MergeID<'src>, NodeIndex>,
}

impl<'src> Graph<'src> {
    pub fn dump_text(&self, scope: ScopedSymbolTable<'src>) -> String {
        let mut visited: Visited<'src> = Visited::default();

        let mut graph: GraphDump = petgraph::Graph::new();

        let SymbolDump {
            immutables,
            mutables,
        } = scope.all_symbols();

        for (name, symbol) in immutables {
            let name = graph.add_node(name.to_string());
            let value = process_data_node(&mut graph, &mut visited, symbol.value);
            graph.add_edge(name, value, "immutable".to_string());
        }
        for (name, symbol) in mutables {
            let name = graph.add_node(name.to_string());
            let value = process_data_node(&mut graph, &mut visited, symbol.value);
            graph.add_edge(name, value, "mutable".to_string());
        }

        process_ctrl_node(&mut graph, &mut visited, self.current_mem.clone());

        dump_cytoscape(&graph)
    }
}

pub fn process_data_node<'src>(
    graph: &mut GraphDump,

    visited: &mut Visited<'src>,
    node: DataID<'src>,
) -> NodeIndex {
    if let Some(idx) = visited.data.get(&node) {
        return *idx;
    }
    use DataKind::*;
    let idx = match node.kind.clone() {
        AtomicType { ty } => graph.add_node(format!("atomic-type {:?}", ty)),
        Literal { literal } => graph.add_node(format!("literal {}", literal)),
        Quote { quote } => graph.add_node(format!(
            "literal \"{}\"",
            tokenizing::with_written_out_escape_sequences(&quote)
        )),
        Boolean(boolean) => graph.add_node(format!("literal {}", boolean)),
        Unit => graph.add_node("unit".to_string()),

        Unary { op, input } => {
            let input = process_data_node(graph, visited, input);
            let op = graph.add_node(format!("operator {}", op));
            graph.add_edge(op, input, "".to_string());
            op
        }
        Binary { op, lhs, rhs } => {
            let lhs = process_data_node(graph, visited, lhs);
            let rhs = process_data_node(graph, visited, rhs);
            let op = graph.add_node(format!("operator {}", op));
            graph.add_edge(op, lhs, "a".to_string());
            graph.add_edge(op, rhs, "b".to_string());
            op
        }
        Load { .. } => todo!(),

        BranchPhi {
            merge,
            when_true,
            when_false,
        } => {
            let merge = process_merge_node(graph, visited, merge);
            let when_true = process_data_node(graph, visited, when_true);
            let when_false = process_data_node(graph, visited, when_false);
            let phi = graph.add_node("phi".to_string());
            graph.add_edge(phi, merge, "merge".to_string());
            graph.add_edge(phi, when_true, "true".to_string());
            graph.add_edge(phi, when_false, "false".to_string());
            phi
        }
        LoopPhi {
            loop_head,
            entry,
            backedge,
        } => {
            todo!();
            let condition = process_data_node(graph, visited, loop_head.condition.clone());
            let entry = process_data_node(graph, visited, entry);
            let backedge = process_data_node(graph, visited, backedge);
            let phi = graph.add_node("loop phi".to_string());
            graph.add_edge(phi, condition, "condition".to_string());
            graph.add_edge(phi, entry, "entry".to_string());
            graph.add_edge(phi, backedge, "backedge".to_string());
            phi
        }

        Unknown => graph.add_node("unknown".to_string()),
    };
    visited.data.insert(node, idx);
    idx
}

pub fn process_merge_node<'src>(
    graph: &mut GraphDump,
    visited: &mut Visited<'src>,
    node: MergeID<'src>,
) -> NodeIndex {
    if let Some(idx) = visited.merge.get(&node) {
        return *idx;
    }

    let idx = {
        let merge_node = graph.add_node(mem!("merge"));
        node.branches.iter().enumerate().for_each(|(i, b)| {
            let branch = process_ctrl_node(graph, visited, b.clone());
            graph.add_edge(merge_node, branch, mem!("{}", i));
        });
        merge_node
    };
    visited.merge.insert(node, idx);
    idx
}

pub fn process_ctrl_node<'src>(
    graph: &mut GraphDump,

    visited: &mut Visited<'src>,
    node: CtrlID<'src>,
) -> NodeIndex {
    if let Some(idx) = visited.ctrl.get(&node) {
        return *idx;
    }

    use CtrlKind::*;
    let idx = match node.kind.clone() {
        Start => graph.add_node(mem!("start")),
        FalseBranch { branch } => {
            let ctrl = process_ctrl_node(graph, visited, branch.ctrl.clone());
            let condition = process_data_node(graph, visited, branch.condition.clone());
            let false_branch = graph.add_node(mem!("false branch"));
            graph.add_edge(false_branch, ctrl, "mem ctrl".to_string());
            graph.add_edge(false_branch, condition, "condition".to_string());
            false_branch
        }
        TrueBranch { branch } => {
            let ctrl = process_ctrl_node(graph, visited, branch.ctrl.clone());
            let condition = process_data_node(graph, visited, branch.condition.clone());
            let true_branch = graph.add_node(mem!("true branch"));
            graph.add_edge(true_branch, ctrl, "mem ctrl".to_string());
            graph.add_edge(true_branch, condition, "condition".to_string());
            true_branch
        }
        Merge { merge } => process_merge_node(graph, visited, merge),
        LoopHead {
            ctrl,
            condition,
            backedge,
        } => {
            let ctrl = process_ctrl_node(graph, visited, ctrl);
            let condition = process_data_node(graph, visited, condition);
            let backedge = process_ctrl_node(graph, visited, backedge);
            let loop_head = graph.add_node(mem!("loop"));
            graph.add_edge(loop_head, ctrl, "mem ctrl".to_string());
            graph.add_edge(loop_head, backedge, "backedge".to_string());
            graph.add_edge(loop_head, condition, "condition".to_string());
            loop_head
        }
        PlaceHolder { ctrl } => {
            let ctrl = process_ctrl_node(graph, visited, ctrl);
            let place_holder = graph.add_node(mem!("place holder"));
            graph.add_edge(place_holder, ctrl, "".to_string());
            place_holder
        }
    };

    visited.ctrl.insert(node, idx);
    idx
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
        } else if let Some(label) = label.strip_prefix("literal") {
            ("literal", label.trim())
        } else if let Some(label) = label.strip_prefix("mem") {
            ("memory", label.trim())
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
