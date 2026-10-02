# Repository Guidelines

## Project Goals
- This is a work in progress compiler for the not existing Rux programming language. (file ending ".rx")
- The goal for Rux is to be a statically typed imperative programming language with 
  - Full compile time execution (future fuel system) + reflection over types 
  - Linear types
  - Rust level safety
- The compiler should be of a "Sea of Nodes" architecture

## Project Architecture && Module Organization
- The compiler is organized into Rust Crates
- Ignore the `base_crate` it contains stale code that will later be recycled into a cli tool
- The compiler is made of passes:
  1. Parsing
    - Tokenization `tokenizer` -> `TokenStream` as interface
    - Parsing `parser` produces an untyped AST right out of a token stream with name table
  2. Graph Construction `graph_builder`
    - The AST gets traversed and translated into a Sea of Nodes graph with full type information
    - Every diagnostic should be collected here
  3. Graph Canonicalization `canonicalizer`
    - The graph gets traversed again, canonicalized and converted into a format that stays canonicalized
  4. Peephole Optimization `optimizer`
    - The graph gets traversed again and locally simplified using a worklist
  5. Scheduling `scheduler` (not yet build)
    - This is will be the final stage of the compiler

## Build, Test, and Development Commands
- `cargo build --release` compiles the toolchain
- `cargo test` runs unit and integration suites; add `-- --ignored` when touching long-running cases.
- `cargo fmt && cargo clippy --all-targets` enforces style and lints; run them before every branch push.

## Coding Style & Naming Conventions
- Follow `rustfmt` defaults (4-space indents, 100-character lines); no tabs.
- Modules and files stay `snake_case`; structs/enums use `CamelCase`; constants use `SCREAMING_SNAKE_CASE`.
- Compile time tokens have the information as their name that they represent: `ScopeIsOpen` 
- Crate names are converted into actor form: `canonicalizion` -> `canonicalizer`
- Prefer explicit lifetimes and `Arc`/`Mutex` wrappers over `unsafe` blocks unless reviewing with another maintainer.

## Testing Guidelines
- Mirror module names in tests: e.g., module-specific tests live in `src/module/test.rs` or inline `mod tests` blocks.
- Use the `#[test] fn parses_basic_block()` naming pattern and document Rux syntax edge cases inline.

## Commit & Pull Request Guidelines
- Match the existing Git history: short, imperative commit subjects such as `"Add SSA lowering"`; describe impact in the body if needed.
- Squash noisy work-in-progress commits before opening a PR.
- PRs must include: summary of behavior change, test plan (`cargo test`, custom scripts), linked issue (if any), and screenshots or CLI transcripts when user-facing output changes.
- Tag another agent familiar with the touched subsystem (parser, VM, docs) and wait for at least one approval before merging.

## Security & Configuration Tips
- Never commit secrets; rely on `.env` files excluded by gitignore and document required keys in `docs/sketch/`.
- When modifying parallel execution features (`threader/`), guard new channels with `cfg(test)` stress tests to avoid nondeterministic panics.
