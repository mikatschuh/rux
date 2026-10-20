use canonicalizer::{DataKind, Graph};
use compiler_benchmarks::{canonical, integer_graph, workloads};

// Follow data dependencies only; merge control and branch conditions are excluded.
fn reachable_data(graph: &Graph, root: canonicalizer::Data) -> usize {
    let mut seen = std::collections::HashSet::new();
    let mut stack = vec![root];
    while let Some(node) = stack.pop() {
        if !seen.insert(graph.direct(node)) {
            continue;
        }
        match &graph[node] {
            DataKind::Unary { value, .. } => stack.push(*value),
            DataKind::Binary { ops, .. } => stack.extend(ops),
            DataKind::ComBinary { ops, .. } => stack.extend(ops.iter()),
            DataKind::Merge { variants, .. } => stack.extend(variants),
            _ => {}
        }
    }
    seen.len()
}
fn report(name: &str, mut graph: Graph, root: canonicalizer::Data) {
    let reachable_before = reachable_data(&graph, root);
    let before = graph.nodes::<DataKind>().count();
    let root_before = format!("{:?}", graph[root]);
    optimizer::optimize_graph(&mut graph);
    let after = graph.nodes::<DataKind>().count();
    let reachable_after = reachable_data(&graph, root);
    println!("{name}: root data dependencies {reachable_before} -> {reachable_after}");
    println!(
        "{name}: canonical data nodes {before} -> {after}\n  root: {root_before} -> {:?}",
        graph[root]
    );
}
fn main() {
    for workload in workloads() {
        if workload.full_pipeline {
            let (graph, roots) = canonical(&workload.source);
            report(&workload.name, graph, roots[0]);
        }
    }
    for size in [16, 64, 256] {
        for (name, foldable) in [("foldable", true), ("unfoldable", false)] {
            let (graph, root) = integer_graph(size, foldable);
            report(&format!("integer_{name}_{size}"), graph, root);
        }
    }
}
