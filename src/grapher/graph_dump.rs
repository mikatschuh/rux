use std::collections::{HashMap, HashSet};
use std::fmt::Write;

use crate::grapher::{Graph, MemNodeID, MemNodeKind, NodeID, NodeKind};

impl<'src> Graph<'src> {
    pub fn dump_text(&self) -> String {
        let mut visited_nodes = HashSet::new();
        let mut visited_mem = HashSet::new();
        let mut nodes = HashMap::new();
        let mut mem = HashMap::new();

        let mut symbol_names: Vec<_> = self.symbols.keys().copied().collect();
        symbol_names.sort_unstable();

        for name in &symbol_names {
            if let Some(symbol) = self.symbols.get(name) {
                self.collect_node_dump(
                    &symbol.ty,
                    &mut visited_nodes,
                    &mut visited_mem,
                    &mut nodes,
                    &mut mem,
                );
                self.collect_node_dump(
                    &symbol.assignment,
                    &mut visited_nodes,
                    &mut visited_mem,
                    &mut nodes,
                    &mut mem,
                );
            }
        }

        self.collect_mem_dump(
            &self.current_mem,
            &mut visited_nodes,
            &mut visited_mem,
            &mut nodes,
            &mut mem,
        );

        let node_indices = Self::make_indices(nodes.keys().copied().collect());
        let mem_indices = Self::make_indices(mem.keys().copied().collect());

        let mut out = String::new();
        writeln!(&mut out, "GraphDump {{").expect("write String");
        writeln!(&mut out, "  symbols:").expect("write String");
        for name in symbol_names {
            let symbol = self.symbols.get(name).expect("symbol exists");
            writeln!(
                &mut out,
                "    {}: ty=n{} value=n{}",
                name,
                node_indices[&Self::node_ptr_id(&symbol.ty)],
                node_indices[&Self::node_ptr_id(&symbol.assignment)]
            )
            .expect("write String");
        }
        writeln!(
            &mut out,
            "  current_mem: m{}",
            mem_indices[&Self::mem_ptr_id(&self.current_mem)]
        )
        .expect("write String");

        let mut node_ids: Vec<_> = nodes.keys().copied().collect();
        node_ids.sort_unstable();
        writeln!(&mut out, "  nodes:").expect("write String");
        for ptr_id in node_ids {
            let node = nodes.get(&ptr_id).expect("node exists");
            writeln!(
                &mut out,
                "    n{} = {}",
                node_indices[&ptr_id],
                self.format_node_dump(node, &node_indices, &mem_indices)
            )
            .expect("write String");
        }

        let mut mem_ids: Vec<_> = mem.keys().copied().collect();
        mem_ids.sort_unstable();
        writeln!(&mut out, "  memory:").expect("write String");
        for ptr_id in mem_ids {
            let mem_node = mem.get(&ptr_id).expect("mem exists");
            writeln!(
                &mut out,
                "    m{} = {}",
                mem_indices[&ptr_id],
                self.format_mem_dump(mem_node, &node_indices, &mem_indices)
            )
            .expect("write String");
        }
        writeln!(&mut out, "}}").expect("write String");

        out
    }

    fn make_indices(mut ptr_ids: Vec<usize>) -> HashMap<usize, usize> {
        ptr_ids.sort_unstable();
        ptr_ids
            .into_iter()
            .enumerate()
            .map(|(index, ptr_id)| (ptr_id, index))
            .collect()
    }

    fn collect_node_dump(
        &self,
        node: &NodeID<'src>,
        visited_nodes: &mut HashSet<usize>,
        visited_mem: &mut HashSet<usize>,
        nodes: &mut HashMap<usize, NodeID<'src>>,
        mem: &mut HashMap<usize, MemNodeID<'src>>,
    ) {
        let node_id = Self::node_ptr_id(node);
        if !visited_nodes.insert(node_id) {
            return;
        }

        nodes.insert(node_id, node.clone());

        match &node.kind {
            NodeKind::Literal { .. }
            | NodeKind::Quote { .. }
            | NodeKind::PrimitiveType { .. }
            | NodeKind::UnInitialized
            | NodeKind::UnknownIdent { .. } => {}
            NodeKind::Unary { input, .. } => {
                self.collect_node_dump(input, visited_nodes, visited_mem, nodes, mem);
            }
            NodeKind::Binary { lhs, rhs, .. } => {
                self.collect_node_dump(lhs, visited_nodes, visited_mem, nodes, mem);
                self.collect_node_dump(rhs, visited_nodes, visited_mem, nodes, mem);
            }
            NodeKind::Load {
                mem: load_mem,
                addr,
            } => {
                self.collect_mem_dump(load_mem, visited_nodes, visited_mem, nodes, mem);
                self.collect_node_dump(addr, visited_nodes, visited_mem, nodes, mem);
            }
            NodeKind::Phi {
                condition,
                when_true,
                when_false,
            } => {
                self.collect_node_dump(condition, visited_nodes, visited_mem, nodes, mem);
                self.collect_node_dump(when_true, visited_nodes, visited_mem, nodes, mem);
                self.collect_node_dump(when_false, visited_nodes, visited_mem, nodes, mem);
            }
        }
    }

    fn collect_mem_dump(
        &self,
        mem_node: &MemNodeID<'src>,
        visited_nodes: &mut HashSet<usize>,
        visited_mem: &mut HashSet<usize>,
        nodes: &mut HashMap<usize, NodeID<'src>>,
        mem: &mut HashMap<usize, MemNodeID<'src>>,
    ) {
        let mem_id = Self::mem_ptr_id(mem_node);
        if !visited_mem.insert(mem_id) {
            return;
        }

        mem.insert(mem_id, mem_node.clone());

        match &mem_node.kind {
            MemNodeKind::ControlFlowStart => {}
            MemNodeKind::LoopHead { entry, backedge } => {
                self.collect_mem_dump(entry, visited_nodes, visited_mem, nodes, mem);
                if let Some(backedge) = backedge {
                    self.collect_mem_dump(backedge, visited_nodes, visited_mem, nodes, mem);
                }
            }
            MemNodeKind::StepClause { prev } => {
                self.collect_mem_dump(prev, visited_nodes, visited_mem, nodes, mem);
            }
            MemNodeKind::Merge {
                condition,
                when_true,
                when_false,
            } => {
                self.collect_node_dump(condition, visited_nodes, visited_mem, nodes, mem);
                self.collect_mem_dump(when_true, visited_nodes, visited_mem, nodes, mem);
                self.collect_mem_dump(when_false, visited_nodes, visited_mem, nodes, mem);
            }
            MemNodeKind::Store { prev, addr, val } => {
                self.collect_mem_dump(prev, visited_nodes, visited_mem, nodes, mem);
                self.collect_node_dump(addr, visited_nodes, visited_mem, nodes, mem);
                self.collect_node_dump(val, visited_nodes, visited_mem, nodes, mem);
            }
        }
    }

    fn format_node_dump(
        &self,
        node: &NodeID<'src>,
        node_indices: &HashMap<usize, usize>,
        mem_indices: &HashMap<usize, usize>,
    ) -> String {
        match &node.kind {
            NodeKind::Literal { literal } => format!("Literal({literal:?})"),
            NodeKind::Quote { quote } => format!("Quote({quote:?})"),
            NodeKind::PrimitiveType { ty } => format!("PrimitiveType({ty:?})"),
            NodeKind::Unary { op, input } => {
                format!(
                    "Unary({op:?}, n{})",
                    node_indices[&Self::node_ptr_id(input)]
                )
            }
            NodeKind::Binary { op, lhs, rhs } => {
                format!(
                    "Binary({op:?}, lhs=n{}, rhs=n{})",
                    node_indices[&Self::node_ptr_id(lhs)],
                    node_indices[&Self::node_ptr_id(rhs)]
                )
            }
            NodeKind::Load { mem, addr } => {
                format!(
                    "Load(mem=m{}, addr=n{})",
                    mem_indices[&Self::mem_ptr_id(mem)],
                    node_indices[&Self::node_ptr_id(addr)]
                )
            }
            NodeKind::UnInitialized => "UnInitialized".to_string(),
            NodeKind::Phi {
                condition,
                when_true,
                when_false,
            } => {
                format!(
                    "Phi(cond=n{}, true=n{}, false=n{})",
                    node_indices[&Self::node_ptr_id(condition)],
                    node_indices[&Self::node_ptr_id(when_true)],
                    node_indices[&Self::node_ptr_id(when_false)]
                )
            }
            NodeKind::UnknownIdent { name } => format!("UnknownIdent({name})"),
        }
    }

    fn format_mem_dump(
        &self,
        mem_node: &MemNodeID<'src>,
        node_indices: &HashMap<usize, usize>,
        mem_indices: &HashMap<usize, usize>,
    ) -> String {
        match &mem_node.kind {
            MemNodeKind::ControlFlowStart => "ControlFlowStart".to_string(),
            MemNodeKind::LoopHead { entry, backedge } => {
                let backedge_text = backedge
                    .as_ref()
                    .map(|edge| format!("m{}", mem_indices[&Self::mem_ptr_id(edge)]))
                    .unwrap_or_else(|| "None".to_string());
                format!(
                    "LoopHead(entry=m{}, backedge={})",
                    mem_indices[&Self::mem_ptr_id(entry)],
                    backedge_text
                )
            }
            MemNodeKind::StepClause { prev } => {
                format!("StepClause(prev=m{})", mem_indices[&Self::mem_ptr_id(prev)])
            }
            MemNodeKind::Merge {
                condition,
                when_true,
                when_false,
            } => {
                format!(
                    "Merge(condition=n{}, a=m{}, b=m{})",
                    node_indices[&Self::node_ptr_id(condition)],
                    mem_indices[&Self::mem_ptr_id(when_true)],
                    mem_indices[&Self::mem_ptr_id(when_false)]
                )
            }
            MemNodeKind::Store { prev, addr, val } => {
                format!(
                    "Store(prev=m{}, addr=n{}, val=n{})",
                    mem_indices[&Self::mem_ptr_id(prev)],
                    node_indices[&Self::node_ptr_id(addr)],
                    node_indices[&Self::node_ptr_id(val)]
                )
            }
        }
    }
}
