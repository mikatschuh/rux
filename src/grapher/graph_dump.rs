use std::collections::HashMap;

use petgraph::{graph::NodeIndex, visit::EdgeRef};

use crate::{
    grapher::{
        Graph,
        graph::{MemID, ValueID, ValueKind},
        symbols::{Scopes, SymbolDump},
    },
    tokenizing,
};

type GraphDump = petgraph::Graph<String, &'static str>;
type VisitedValueNodes<'src> = HashMap<ValueID<'src>, NodeIndex>;
type VisitedMemoryNodes<'src> = HashMap<MemID<'src>, NodeIndex>;

impl<'src> Graph<'src> {
    pub fn dump_text(&self, scope: Scopes<'src>) -> String {
        let mut visited_value_nodes: VisitedValueNodes<'src> = HashMap::new();
        let mut visited_memory_nodes: VisitedMemoryNodes<'src> = HashMap::new();

        let mut graph: GraphDump = petgraph::Graph::new();

        let SymbolDump {
            constants,
            variables,
            mutables,
        } = scope.all_symbols();

        for (name, symbol) in constants {
            let name = graph.add_node(name.to_string());
            let value = process_value_node(
                &mut graph,
                &mut visited_value_nodes,
                &mut visited_memory_nodes,
                symbol.assignment,
            );
            graph.add_edge(name, value, "constant");
        }
        for (name, symbol) in variables {
            let name = graph.add_node(name.to_string());
            let value = process_value_node(
                &mut graph,
                &mut visited_value_nodes,
                &mut visited_memory_nodes,
                symbol.assignment,
            );
            graph.add_edge(name, value, "variable");
        }
        for (name, symbol) in mutables {
            let name = graph.add_node(name.to_string());
            let value = process_value_node(
                &mut graph,
                &mut visited_value_nodes,
                &mut visited_memory_nodes,
                symbol.assignment,
            );
            graph.add_edge(name, value, "mutable");
        }

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
            "literal {}",
            tokenizing::with_written_out_escape_sequences(&quote)
        )),
        Boolean(boolean) => graph.add_node(format!("literal {}", boolean)),
        Unit => graph.add_node("unit".to_string()),

        Unary { op, input } => {
            let input = process_value_node(graph, visited_value_nodes, visited_memory_nodes, input);
            let op = graph.add_node(format!("operator {}", op));
            graph.add_edge(op, input, "1");
            op
        }
        Binary { op, lhs, rhs } => {
            let lhs = process_value_node(graph, visited_value_nodes, visited_memory_nodes, lhs);
            let rhs = process_value_node(graph, visited_value_nodes, visited_memory_nodes, rhs);
            let op = graph.add_node(format!("operator {}", op));
            graph.add_edge(op, lhs, "1");
            graph.add_edge(op, rhs, "2");
            op
        }
        Load { mem, addr } => todo!(),

        UnInitialized => graph.add_node("unitialized".to_string()),

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
        } else {
            ("variable", label.as_ref())
        };
        out += &format!(
            "{{ data: {{ id: '{}', label: '{}', group: '{}' }} }},\n",
            idx.index(),
            label.replace('\'', "\\'").replace('\\', "\\\\"),
            group
        );
    }

    for edge in g.edge_references() {
        out += &format!(
            "{{ data: {{ source: '{}', target: '{}', label: '{}' }} }},\n",
            edge.source().index(),
            edge.target().index(),
            edge.weight().replace('\'', "\\'").replace('\\', "\\\\")
        );
    }

    out
}
