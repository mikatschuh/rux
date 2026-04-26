use std::collections::HashMap;

use petgraph::{graph::NodeIndex, visit::EdgeRef};

use crate::{
    grapher::{
        Graph,
        graph::{MemID, MemKind, ValueID, ValueKind},
        parser::{ScopedSymbolTable, SymbolDump},
    },
    tokenizing,
};

type GraphDump = petgraph::Graph<String, &'static str>;
type VisitedValueNodes<'src> = HashMap<ValueID<'src>, NodeIndex>;
type VisitedMemoryNodes<'src> = HashMap<MemID<'src>, NodeIndex>;

impl<'src> Graph<'src> {
    pub fn dump_text(&self, scope: ScopedSymbolTable<'src>) -> String {
        let mut visited_value_nodes: VisitedValueNodes<'src> = HashMap::new();
        let mut visited_memory_nodes: VisitedMemoryNodes<'src> = HashMap::new();

        let mut graph: GraphDump = petgraph::Graph::new();

        let SymbolDump {
            immutables,
            mutables,
        } = scope.all_symbols();

        for (name, symbol) in immutables {
            let name = graph.add_node(name.to_string());
            let value = process_value_node(
                &mut graph,
                &mut visited_value_nodes,
                &mut visited_memory_nodes,
                symbol.value,
            );
            graph.add_edge(name, value, "immutable");
        }
        for (name, symbol) in mutables {
            let name = graph.add_node(name.to_string());
            let value = process_value_node(
                &mut graph,
                &mut visited_value_nodes,
                &mut visited_memory_nodes,
                symbol.value,
            );
            graph.add_edge(name, value, "mutable");
        }

        process_memory_node(
            &mut graph,
            &mut visited_value_nodes,
            &mut visited_memory_nodes,
            self.current_mem.clone(),
        );

        dump_cytoscape(&graph)
    }
}

pub fn process_value_node<'src>(
    graph: &mut GraphDump,

    visited_value_nodes: &mut VisitedValueNodes<'src>,
    visited_memory_nodes: &mut VisitedMemoryNodes<'src>,
    node: ValueID<'src>,
) -> NodeIndex {
    if let Some(idx) = visited_value_nodes.get(&node) {
        return *idx;
    }
    use ValueKind::*;
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
            let input = process_value_node(graph, visited_value_nodes, visited_memory_nodes, input);
            let op = graph.add_node(format!("operator {}", op));
            graph.add_edge(op, input, "");
            op
        }
        Binary { op, lhs, rhs } => {
            let lhs = process_value_node(graph, visited_value_nodes, visited_memory_nodes, lhs);
            let rhs = process_value_node(graph, visited_value_nodes, visited_memory_nodes, rhs);
            let op = graph.add_node(format!("operator {}", op));
            graph.add_edge(op, lhs, "a");
            graph.add_edge(op, rhs, "b");
            op
        }
        Load { .. } => todo!(),

        Phi {
            condition,
            when_true,
            when_false,
        } => {
            let condition =
                process_value_node(graph, visited_value_nodes, visited_memory_nodes, condition);
            let when_true =
                process_value_node(graph, visited_value_nodes, visited_memory_nodes, when_true);
            let when_false =
                process_value_node(graph, visited_value_nodes, visited_memory_nodes, when_false);
            let phi = graph.add_node("phi".to_string());
            graph.add_edge(phi, condition, "condition");
            graph.add_edge(phi, when_true, "true");
            graph.add_edge(phi, when_false, "false");
            phi
        }

        Unknown => graph.add_node("unknown".to_string()),
    };
    visited_value_nodes.insert(node, idx);
    idx
}

pub fn process_memory_node<'src>(
    graph: &mut GraphDump,

    visited_value_nodes: &mut VisitedValueNodes<'src>,
    visited_memory_nodes: &mut VisitedMemoryNodes<'src>,
    node: MemID<'src>,
) -> NodeIndex {
    macro_rules! mem {
        ($($arg:tt)*) => {
            format!("mem {}", format!($($arg)*))
        };
        () => {

        }
    }

    if let Some(idx) = visited_memory_nodes.get(&node) {
        return *idx;
    }

    use MemKind::*;
    let idx = match node.kind.clone() {
        Start => graph.add_node(mem!("start")),
        FalseBranch { branch } => {
            let ctrl = process_memory_node(
                graph,
                visited_value_nodes,
                visited_memory_nodes,
                branch.ctrl.clone(),
            );
            let condition = process_value_node(
                graph,
                visited_value_nodes,
                visited_memory_nodes,
                branch.condition.clone(),
            );
            let false_branch = graph.add_node(mem!("false branch"));
            graph.add_edge(false_branch, ctrl, "mem ctrl");
            graph.add_edge(false_branch, condition, "condition");
            false_branch
        }
        TrueBranch { branch } => {
            let ctrl = process_memory_node(
                graph,
                visited_value_nodes,
                visited_memory_nodes,
                branch.ctrl.clone(),
            );
            let condition = process_value_node(
                graph,
                visited_value_nodes,
                visited_memory_nodes,
                branch.condition.clone(),
            );
            let true_branch = graph.add_node(mem!("true branch"));
            graph.add_edge(true_branch, ctrl, "mem ctrl");
            graph.add_edge(true_branch, condition, "condition");
            true_branch
        }
        Merge { a, b } => {
            let a = process_memory_node(graph, visited_value_nodes, visited_memory_nodes, a);
            let b = process_memory_node(graph, visited_value_nodes, visited_memory_nodes, b);
            let merge = graph.add_node(mem!("merge"));
            graph.add_edge(merge, a, "mem a");
            graph.add_edge(merge, b, "mem b");
            merge
        }
        LoopHead {
            ctrl,
            condition,
            backedge,
        } => {
            let ctrl = process_memory_node(graph, visited_value_nodes, visited_memory_nodes, ctrl);
            let condition =
                process_value_node(graph, visited_value_nodes, visited_memory_nodes, condition);
            let backedge =
                process_memory_node(graph, visited_value_nodes, visited_memory_nodes, backedge);
            let loop_head = graph.add_node(mem!("loop"));
            graph.add_edge(loop_head, ctrl, "mem ctrl");
            graph.add_edge(loop_head, backedge, "backedge");
            graph.add_edge(loop_head, condition, "condition");
            loop_head
        }
        PlaceHolder { ctrl } => {
            let ctrl = process_memory_node(graph, visited_value_nodes, visited_memory_nodes, ctrl);
            let place_holder = graph.add_node(mem!("place holder"));
            graph.add_edge(place_holder, ctrl, "");
            place_holder
        }
    };

    visited_memory_nodes.insert(node, idx);
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
