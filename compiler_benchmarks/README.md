# Compiler pass benchmarks

A first short run with per-pass confidence intervals is saved in [RESULTS.md](RESULTS.md).

Run from the workspace root:

```sh
cargo bench -p compiler_benchmarks --bench passes
# Filter to a workload, or to one pass across workloads:
cargo bench -p compiler_benchmarks --bench passes -- boolean_chain_64
cargo bench -p compiler_benchmarks --bench passes -- canonicalize
# Fast smoke measurement (use the defaults for comparisons):
cargo bench -p compiler_benchmarks --bench passes -- --warm-up-time 0.1 --measurement-time 0.2 --sample-size 10
```

Criterion prints a time interval per pass and source-byte throughput, saves
machine-readable estimates in `target/criterion/<workload>/<pass>/new/estimates.json`,
and creates an HTML index at `target/criterion/report/index.html`. Default runs
use 50 samples, one second of warmup and three seconds of measurement per case.
Use `--save-baseline NAME` and later `--baseline NAME` to compare changes.
These are statistical wall-clock estimates on your machine, not exact or
portable costs. Close competing workloads when comparing runs.

## Timing boundaries

| Benchmark | Timed work |
| --- | --- |
| `tokenize` | Create and completely consume the lazy tokenizer, including interning |
| `parse_only` | Construct AST and item table from replayed tokens; excludes tokenization |
| `build_graph` | Build the typed source graph from a prepared AST; excludes HTML dumping |
| `canonicalize` | Convert a prepared source graph from its result root |
| `peephole` | Optimize a fresh, unoptimized canonical graph |
| `frontend_end_to_end` | Lazy tokenization, parsing and graph construction |
| `end_to_end` | All implemented passes, including their intermediate cleanup |

Source generation, file loading, prerequisite passes, and final output drops
are outside isolated pass timings. Criterion's batched loops separate setup
from measurement; optimizer setup rebuilds the graph for every iteration
because reusing an optimized graph measures a different workload. Parser-only
setup preserves token spans, EOF position and the original symbol interner.
Consuming APIs still include cleanup they perform internally (including the
replay token buffer). The replay stream differs from the production lazy
stream, so isolated times should not be added to predict end-to-end time.
Canonicalization visits the graph reachable from `main`; unused bindings may
never reach later passes. The optimizer currently seeds all four node-kind
worklists but processes data nodes only; its timing includes that overhead.
No scheduling or execution is measured: neither is available yet.

## Workloads and language limits

The four new `.rx` examples beside `graph_builder/examples/test.rx` use only
constant entry blocks, local bindings, assignment, operators, `if`, `loop`, and
`break`. There are no functions, calls, arrays, memory, or I/O. `sum_loop.rx`
and `redundant_arithmetic.rx` complement the original Fibonacci example.
`boolean_merges.rx` exercises repeated expressions and equivalent branch
values; `boolean_loop.rx` exercises a cycle and a redundant boolean identity.

Numeric literals currently trigger a `todo!` in canonicalization, so numeric
source cases intentionally stop after graph construction. Boolean examples
run through the full pipeline. This suite does not silently change language
semantics to get numeric programs through unsupported lowering.

Deterministic arithmetic and boolean dependency chains are generated in memory
at sizes 16, 64, and 256. Every final binding is returned to keep the chain live.
The boolean chain should fold to `true` at these even sizes. Arithmetic values
are bounded with modulo, avoiding intentional overflow or division by zero.
To inspect or save generated source:

```sh
cargo run -q -p compiler_benchmarks --example generate -- boolean 64
cargo run -q -p compiler_benchmarks --example generate -- arithmetic 256 > /tmp/arithmetic.rx
```

`integer_peephole/{foldable,unfoldable}/{16,64,256}` bypasses missing literal
lowering with directly constructed canonical graphs. The foldable addition
chain should become the literal `size + 1`. The other starts with unsupported
negation and should remain a chain, providing a comparison with little
successful rewriting. These are optimizer microbenchmarks, not source pipeline
benchmarks; throughput is chain elements rather than source bytes.

## Inspect optimizer behavior

```sh
cargo run -p compiler_benchmarks --example optimizer_report
cargo test -p compiler_benchmarks
```

The report prints root data dependency counts before/after, stored canonical
data node counts, and the root node before/after. Root dependency counts follow
only data edges (including merge variants), excluding control and branch
condition edges. Stored counts are not live-code size: aliases and unused
nodes remain stored, and folding may allocate constants. Constant chains can
collapse to a single root value without reducing stored node counts.

Current gaps are visible in these workloads: commutative identity rules exist
but are not called, negation and reciprocal folding are absent, and control
branches are not simplified. Constant branch conditions alone therefore do
not imply an entire conditional or loop disappears.

Tests reject tokenizer/parser/builder diagnostics, compare ASTs from lazy and
replayed token streams, compile every checked-in and generated workload to its
supported stage, and verify successful and unsuccessful integer folding.
Timing methodology follows Criterion's [batched timing documentation](https://docs.rs/criterion/0.8.2/criterion/struct.Bencher.html#method.iter_batched_ref).
