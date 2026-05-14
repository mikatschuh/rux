# Repository Guidelines

## Project Goals
- This is a work in progress compiler for the not existing Rux programming language. (file ending ".rx")
- The goal for Rux is to be a statically typed low level imperative programming language with a strong emphasis on metaprogramming and compile time safety
- The compiler should be a "Sea of Nodes" compiler

## Project Architecture 
- The compiler has two stages as of right now: 
  1. source code gets tokenized on the fly and parsed into an untyped AST collecting all the items in the file scope
  2. the AST gets traversed and translated into a Sea of Nodes graph with full type information

## Project Structure & Module Organization
- `src/` hosts the Rust compiler
- tokenizing work in `tokenizing/`,
- to understand the tokenizing api a look at `src/tokenizing/test.rs` and the `TokenStream` trait in 
`src/tokenizing/mod.rs` can be taken (peek/consume based)
- `src/parser` contains a parser for the language which produces an AST (`src/parser/ast.rs`) which gets then fed into the Sea of Nodes module
- `src/grapher` contains the Sea of Nodes framework in `graph.rs`, the AST translator in `mod.rs` and in `test.rs` the tests 
- `codegen/`, `vms/` and `interpreter/` are yet to be filled with content in future sessions
- `test-project/` is the sample Rux workspace (`test.rx`) used by integration tests and quick demos.
- `docs/` contains design references on Rux but is completely outdated and has to be ignored.

## Build, Test, and Development Commands
- `cargo build --release` compiles the toolchain; `cargo run -- build my_project/inter.rx` compiles the demo program end-to-end.
- `cargo test` runs unit and integration suites; add `-- --ignored` when touching long-running cases.
- `cargo fmt && cargo clippy --all-targets` enforces style and lints; run them before every branch push.
- `./watcher.sh` is for humans and can be ignored

## Coding Style & Naming Conventions
- Follow `rustfmt` defaults (4-space indents, 100-character lines); no tabs.
- Modules and files stay `snake_case`; structs/enums use `CamelCase`; constants use `SCREAMING_SNAKE_CASE`.
- Prefer explicit lifetimes and `Arc`/`Mutex` wrappers over `unsafe` blocks unless reviewing with another maintainer.

## Testing Guidelines
- Mirror module names in tests: e.g., module-specific tests live in `src/module/test.rs` or inline `mod tests` blocks.
- Use the `#[test] fn parses_basic_block()` naming pattern and document Rux syntax edge cases inline.
- Keep coverage high on tokenizer, grapher

## Commit & Pull Request Guidelines
- Match the existing Git history: short, imperative commit subjects such as `"Add SSA lowering"`; describe impact in the body if needed.
- Squash noisy work-in-progress commits before opening a PR.
- PRs must include: summary of behavior change, test plan (`cargo test`, custom scripts), linked issue (if any), and screenshots or CLI transcripts when user-facing output changes.
- Tag another agent familiar with the touched subsystem (parser, VM, docs) and wait for at least one approval before merging.

## Security & Configuration Tips
- Never commit secrets; rely on `.env` files excluded by gitignore and document required keys in `docs/sketch/`.
- When modifying parallel execution features (`threader/`), guard new channels with `cfg(test)` stress tests to avoid nondeterministic panics.
