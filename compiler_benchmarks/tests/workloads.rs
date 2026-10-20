use canonicalizer::DataKind;
use compiler_benchmarks::{NoErrors, Tokens, build, integer_graph, parse, workloads};

#[test]
fn compiles_all_workloads_and_replayed_tokens() {
    for workload in workloads() {
        let mut lazy = parse(&workload.source);
        let mut replayed = parser::parse(Tokens::new(&workload.source), NoErrors);
        assert_eq!(
            format!("{:?}", lazy.ast),
            format!("{:?}", replayed.ast),
            "{}",
            workload.name
        );
        let (graph, root) = build(&mut lazy);
        let _ = build(&mut replayed);
        if workload.full_pipeline {
            let (mut graph, roots) = canonicalizer::process_graph(&graph, [root]);
            optimizer::optimize_graph(&mut graph);
            if workload.name.starts_with("boolean_chain_") {
                assert_eq!(graph[roots[0]], DataKind::Boolean(true));
            }
        }
    }
}

#[test]
fn folds_integer_chains_but_preserves_unsupported_negation() {
    for size in [16, 64, 256] {
        let (mut graph, root) = integer_graph(size, true);
        optimizer::optimize_graph(&mut graph);
        assert_eq!(graph[root], DataKind::Literal(size + 1));
        let (mut graph, root) = integer_graph(size, false);
        let before = graph.nodes::<DataKind>().count();
        optimizer::optimize_graph(&mut graph);
        assert_eq!(graph.nodes::<DataKind>().count(), before);
        assert!(matches!(graph[root], DataKind::ComBinary { .. }));
    }
}
