use std::collections::{HashMap, HashSet};
use std::fmt::Write;

use crate::grapher::{Graph, MemNodeID, MemNodeKind, NodeID, NodeKind};

impl<'src> Graph<'src> {
    pub fn dump_text(&self) -> String {
        let mut visited_nodes = HashSet::new();
        let mut visited_mem = HashSet::new();
        let mut node_lines = HashMap::new();
        let mut mem_lines = HashMap::new();

        let mut symbol_names: Vec<_> = self.symbols.keys().copied().collect();
        symbol_names.sort_unstable();

        for name in &symbol_names {
            if let Some(symbol) = self.symbols.get(name) {
                self.collect_node_dump(
                    &symbol.ty,
                    &mut visited_nodes,
                    &mut visited_mem,
                    &mut node_lines,
                    &mut mem_lines,
                );
                self.collect_node_dump(
                    &symbol.assignment,
                    &mut visited_nodes,
                    &mut visited_mem,
                    &mut node_lines,
                    &mut mem_lines,
                );
            }
        }

        self.collect_mem_dump(
            &self.current_mem,
            &mut visited_nodes,
            &mut visited_mem,
            &mut node_lines,
            &mut mem_lines,
        );

        let mut out = String::new();
        writeln!(&mut out, "GraphDump {{").expect("write String");
        writeln!(&mut out, "  symbols:").expect("write String");
        for name in symbol_names {
            let symbol = self.symbols.get(name).expect("symbol exists");
            writeln!(
                &mut out,
                "    {}: ty=n{} value=n{}",
                name,
                Self::node_ptr_id(&symbol.ty),
                Self::node_ptr_id(&symbol.assignment)
            )
            .expect("write String");
        }
        writeln!(
            &mut out,
            "  current_mem: m{}",
            Self::mem_ptr_id(&self.current_mem)
        )
        .expect("write String");

        let mut node_ids: Vec<_> = node_lines.keys().copied().collect();
        node_ids.sort_unstable();
        writeln!(&mut out, "  nodes:").expect("write String");
        for id in node_ids {
            let line = node_lines.get(&id).expect("node line");
            writeln!(&mut out, "    n{} = {}", id, line).expect("write String");
        }

        let mut mem_ids: Vec<_> = mem_lines.keys().copied().collect();
        mem_ids.sort_unstable();
        writeln!(&mut out, "  memory:").expect("write String");
        for id in mem_ids {
            let line = mem_lines.get(&id).expect("mem line");
            writeln!(&mut out, "    m{} = {}", id, line).expect("write String");
        }
        writeln!(&mut out, "}}").expect("write String");

        out
    }

    fn collect_node_dump(
        &self,
        node: &NodeID<'src>,
        visited_nodes: &mut HashSet<usize>,
        visited_mem: &mut HashSet<usize>,
        node_lines: &mut HashMap<usize, String>,
        mem_lines: &mut HashMap<usize, String>,
    ) {
        let node_id = Self::node_ptr_id(node);
        if !visited_nodes.insert(node_id) {
            return;
        }

        let line = match &node.kind {
            NodeKind::MemoryNode { mem } => match &mem.kind {
                MemNodeKind::Load { prev, addr } => {
                    self.collect_mem_dump(mem, visited_nodes, visited_mem, node_lines, mem_lines);
                    format!("Load(mem=m{})", Self::mem_ptr_id(&mem))
                }
                _ => {
                    panic!("only MemNodeKind::Load is expected right now ")
                }
            },
            NodeKind::Literal { literal } => format!("Literal({literal:?})"),
            NodeKind::Quote { quote } => format!("Quote({quote:?})"),
            NodeKind::PrimitiveType { ty } => format!("PrimitiveType({ty:?})"),
            NodeKind::Unary { op, input } => {
                self.collect_node_dump(input, visited_nodes, visited_mem, node_lines, mem_lines);
                format!("Unary({op:?}, n{})", Self::node_ptr_id(input))
            }
            NodeKind::Binary { op, lhs, rhs } => {
                self.collect_node_dump(lhs, visited_nodes, visited_mem, node_lines, mem_lines);
                self.collect_node_dump(rhs, visited_nodes, visited_mem, node_lines, mem_lines);
                format!(
                    "Binary({op:?}, lhs=n{}, rhs=n{})",
                    Self::node_ptr_id(lhs),
                    Self::node_ptr_id(rhs)
                )
            }
            NodeKind::UnInitialized => "UnInitialized".to_string(),
            NodeKind::Phi {
                condition,
                when_true,
                when_false,
            } => {
                self.collect_node_dump(
                    condition,
                    visited_nodes,
                    visited_mem,
                    node_lines,
                    mem_lines,
                );
                self.collect_node_dump(
                    when_true,
                    visited_nodes,
                    visited_mem,
                    node_lines,
                    mem_lines,
                );
                self.collect_node_dump(
                    when_false,
                    visited_nodes,
                    visited_mem,
                    node_lines,
                    mem_lines,
                );
                format!(
                    "Phi(cond=n{}, true=n{}, false=n{})",
                    Self::node_ptr_id(condition),
                    Self::node_ptr_id(when_true),
                    Self::node_ptr_id(when_false)
                )
            }
            NodeKind::UnknownIdent { name } => format!("UnknownIdent({name})"),
        };

        node_lines.insert(node_id, line);
    }

    fn collect_mem_dump(
        &self,
        mem: &MemNodeID<'src>,
        visited_nodes: &mut HashSet<usize>,
        visited_mem: &mut HashSet<usize>,
        node_lines: &mut HashMap<usize, String>,
        mem_lines: &mut HashMap<usize, String>,
    ) {
        let mem_id = Self::mem_ptr_id(mem);
        if !visited_mem.insert(mem_id) {
            return;
        }

        let line = match &mem.kind {
            MemNodeKind::ControlFlowStart => "ControlFlowStart".to_string(),
            MemNodeKind::LoopHead { entry, backedge } => {
                self.collect_mem_dump(entry, visited_nodes, visited_mem, node_lines, mem_lines);
                if let Some(backedge) = backedge {
                    self.collect_mem_dump(
                        backedge,
                        visited_nodes,
                        visited_mem,
                        node_lines,
                        mem_lines,
                    );
                }
                let backedge_text = backedge
                    .as_ref()
                    .map(|edge| format!("m{}", Self::mem_ptr_id(edge)))
                    .unwrap_or_else(|| "None".to_string());
                format!(
                    "LoopHead(entry=m{}, backedge={})",
                    Self::mem_ptr_id(entry),
                    backedge_text
                )
            }
            MemNodeKind::Merge { condition, a, b } => {
                self.collect_node_dump(
                    condition,
                    visited_nodes,
                    visited_mem,
                    node_lines,
                    mem_lines,
                );
                self.collect_mem_dump(a, visited_nodes, visited_mem, node_lines, mem_lines);
                self.collect_mem_dump(b, visited_nodes, visited_mem, node_lines, mem_lines);
                format!(
                    "Merge(condition=n{}, a=m{}, b=m{})",
                    Self::node_ptr_id(condition),
                    Self::mem_ptr_id(a),
                    Self::mem_ptr_id(b)
                )
            }
            MemNodeKind::Store { prev, addr, val } => {
                self.collect_mem_dump(prev, visited_nodes, visited_mem, node_lines, mem_lines);
                self.collect_node_dump(addr, visited_nodes, visited_mem, node_lines, mem_lines);
                self.collect_node_dump(val, visited_nodes, visited_mem, node_lines, mem_lines);
                format!(
                    "Store(prev=m{}, addr=n{}, val=n{})",
                    Self::mem_ptr_id(prev),
                    Self::node_ptr_id(addr),
                    Self::node_ptr_id(val)
                )
            }
            MemNodeKind::Load { prev, addr } => {
                self.collect_mem_dump(prev, visited_nodes, visited_mem, node_lines, mem_lines);
                self.collect_node_dump(addr, visited_nodes, visited_mem, node_lines, mem_lines);
                format!(
                    "Load(prev=m{}, addr=n{})",
                    Self::mem_ptr_id(prev),
                    Self::node_ptr_id(addr)
                )
            }
        };

        mem_lines.insert(mem_id, line);
    }
}
