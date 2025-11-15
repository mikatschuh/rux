# Repository Guidelines

## Project Goals
- This is a work in progress compiler for the not existing Flou (working name) programming language. (file ending ".rx")
- The goal for Flou is to be a statically typed low level imperative programming language with a strong emphasis on metaprogramming and compile time safety
- The compiler should be a tree less multithreaded "Sea of Nodes" compiler

## Project Structure & Module Organization
- `src/` hosts the Rust compiler
- tokenizing work in `tokenizing/`,
- Dependency Injection is used via the `TokenStream` trait found in tokenizing
- a function needing a tokenstream should just use the `tokens: &mut impl TokenStream` pattern
- to understand the tokenizing api a look at `src/tokenizing/test.rs` and the `TokenStream` trait in 
`src/tokenizing/mod.rs` can be taken
- `src/parser` is deprecated and should be stayed independent from
- `codegen/`, `vms/` and `interpreter/` are yet to be filled with content in future sessions
- `my_project/` is the sample Flou workspace (`main.rx`, `module/inter.rx`) used by integration tests and quick demos.
- `docs/` contains design references on Flou but is partially outdated and should be ignored.

## Build, Test, and Development Commands
- `cargo build --release` compiles the toolchain; `cargo run -- build my_project/inter.rx` compiles the demo program        end-to-end.
- `cargo test` runs unit and integration suites; add `-- --ignored` when touching long-running cases.
- `cargo fmt && cargo clippy --all-targets` enforces style and lints; run them before every branch push.
- `./watcher.sh` is for humans and can be ignored

## Coding Style & Naming Conventions
- Follow `rustfmt` defaults (4-space indents, 100-character lines); no tabs.
- Modules and files stay `snake_case`; structs/enums use `CamelCase`; constants use `SCREAMING_SNAKE_CASE`.
- Prefer explicit lifetimes and `Arc`/`Mutex` wrappers over `unsafe` blocks unless reviewing with another maintainer.

## Testing Guidelines
- Mirror module names in tests: e.g., parser-specific tests live in `src/parser/tests.rs` or inline `mod tests` blocks.
- Use the `#[test] fn parses_basic_block()` naming pattern and document Flou syntax edge cases inline.
- Keep coverage high on tokenizer, parser, and VM boundary layers; new opcodes require golden samples in `docs/examples/` plus runtime assertions.

## Commit & Pull Request Guidelines
- Match the existing Git history: short, imperative commit subjects such as `"Add SSA lowering"`; describe impact in the body if needed.
- Squash noisy work-in-progress commits before opening a PR.
- PRs must include: summary of behavior change, test plan (`cargo test`, custom scripts), linked issue (if any), and screenshots or CLI transcripts when user-facing output changes.
- Tag another agent familiar with the touched subsystem (parser, VM, docs) and wait for at least one approval before merging.

## Security & Configuration Tips
- Never commit secrets; rely on `.env` files excluded by gitignore and document required keys in `docs/sketch/`.
- When modifying parallel execution features (`threader/`), guard new channels with `cfg(test)` stress tests to avoid nondeterministic panics.
