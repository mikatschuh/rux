use std::{hint::black_box, time::Duration};

use compiler_benchmarks::{
    NoErrors, POINTER_SIZE, Tokens, build, canonical, integer_graph, parse, workloads,
};
use criterion::{BatchSize, BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use tokenizer::Tokenizer;

fn passes(c: &mut Criterion) {
    for workload in workloads() {
        let source = workload.source.as_str();
        // Validate before entering any timing loop, including numeric frontend cases.
        let _ = build(&mut parse(source));
        let mut group = c.benchmark_group(&workload.name);
        group.throughput(Throughput::Bytes(source.len() as u64));
        group.bench_function("tokenize", |b| {
            b.iter_batched(
                || (),
                |()| {
                    let mut tokens = Tokenizer::new(black_box(source), NoErrors, POINTER_SIZE);
                    for token in &mut tokens {
                        black_box(token);
                    }
                    tokens
                },
                BatchSize::LargeInput,
            );
        });
        group.bench_function("parse_only", |b| {
            b.iter_batched(
                || Tokens::new(source),
                |tokens| parser::parse(black_box(tokens), NoErrors),
                BatchSize::LargeInput,
            );
        });
        group.bench_function("build_graph", |b| {
            b.iter_batched_ref(
                || parse(source),
                |parsed| build(black_box(parsed)),
                BatchSize::LargeInput,
            );
        });
        group.bench_function("frontend_end_to_end", |b| {
            b.iter_batched(
                || (),
                |()| build(&mut parse(black_box(source))),
                BatchSize::LargeInput,
            );
        });
        if workload.full_pipeline {
            group.bench_function("canonicalize", |b| {
                b.iter_batched_ref(
                    || build(&mut parse(source)),
                    |(graph, root)| canonicalizer::process_graph(black_box(graph), [*root]),
                    BatchSize::LargeInput,
                );
            });
            group.bench_function("peephole", |b| {
                b.iter_batched_ref(
                    || canonical(source),
                    |(graph, _)| optimizer::optimize_graph(black_box(graph)),
                    BatchSize::LargeInput,
                );
            });
            group.bench_function("end_to_end", |b| {
                b.iter_batched(
                    || (),
                    |()| {
                        let (mut graph, roots) = canonical(black_box(source));
                        optimizer::optimize_graph(&mut graph);
                        (graph, roots)
                    },
                    BatchSize::LargeInput,
                );
            });
        }
        group.finish();
    }
    let mut group = c.benchmark_group("integer_peephole");
    for size in [16, 64, 256] {
        group.throughput(Throughput::Elements(size as u64));
        for (name, foldable) in [("foldable", true), ("unfoldable", false)] {
            group.bench_with_input(BenchmarkId::new(name, size), &size, |b, &size| {
                b.iter_batched_ref(
                    || integer_graph(size, foldable),
                    |(graph, _)| optimizer::optimize_graph(black_box(graph)),
                    BatchSize::LargeInput,
                );
            });
        }
    }
    group.finish();
}

fn configuration() -> Criterion {
    Criterion::default()
        .output_directory(
            &std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../target/criterion"),
        )
        .sample_size(50)
        .warm_up_time(Duration::from_secs(1))
        .measurement_time(Duration::from_secs(3))
}

criterion_group! {
    name = benches;
    config = configuration();
    targets = passes
}
criterion_main!(benches);
