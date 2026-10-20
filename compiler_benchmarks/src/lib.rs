//! Shared, deterministic workloads and untimed setup for compiler benchmarks.
use std::fmt::Write;

use canonicalizer::{ComBinaryOp, Data, DataKind, Graph, OrderedOps};
use tokenizer::{Interner, Span, Token, TokenStream, Tokenizer};

pub const POINTER_SIZE: tokenizer::TypeSize = 64;

/// Fail immediately: timing error recovery would produce misleading results.
pub struct NoErrors;
macro_rules! diagnostics {
    ($($module:ident),*) => {$(
        impl $module::Diagnostics for NoErrors {
            fn add(&mut self, span: Span, error: $module::Error) {
                panic!("{} diagnostic at {span:?}: {error:?}", stringify!($module));
            }
        }
    )*};
}
diagnostics!(tokenizer, parser, graph_builder);

/// Replay preserves token spans, EOF position, and the original symbol interner.
/// Construct outside the parser timing loop to exclude lazy tokenization.
pub struct Tokens<'src> {
    tokens: std::vec::IntoIter<(Token<'src>, Span)>,
    eof: Span,
    interner: Interner<'src>,
}
impl<'src> Tokens<'src> {
    pub fn new(source: &'src str) -> Self {
        let mut tokenizer = Tokenizer::new(source, NoErrors, POINTER_SIZE);
        let mut tokens = Vec::new();
        while tokenizer.peek().is_some() {
            let span = tokenizer.pos();
            tokens.push((tokenizer.next().unwrap(), span));
        }
        let eof = tokenizer.pos();
        let (interner, _) = tokenizer.into_parts();
        Self {
            tokens: tokens.into_iter(),
            eof,
            interner,
        }
    }
}
impl<'src> Iterator for Tokens<'src> {
    type Item = Token<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.next().map(|(token, _)| token)
    }
}
impl<'src> TokenStream<'src> for Tokens<'src> {
    type DiagnosticsStack = NoErrors;
    fn peek(&self) -> Option<&Token<'src>> {
        self.tokens.as_slice().first().map(|(token, _)| token)
    }
    fn pos(&self) -> Span {
        self.tokens
            .as_slice()
            .first()
            .map_or(self.eof, |(_, span)| *span)
    }
    fn into_parts(self) -> (Interner<'src>, NoErrors) {
        (self.interner, NoErrors)
    }
}

pub type Parsed<'src> = parser::ParserOutput<'src, NoErrors, NoErrors>;
pub fn parse(source: &str) -> Parsed<'_> {
    parser::parse(Tokenizer::new(source, NoErrors, POINTER_SIZE), NoErrors)
}
pub fn build<'src>(parsed: &mut Parsed<'src>) -> (graph_builder::Graph<'src>, graph_builder::Data) {
    let main = parsed.interner.get("main");
    let (graph, root, _) = graph_builder::build_graph(
        &parsed.ast,
        std::mem::take(&mut parsed.item_table),
        main,
        NoErrors,
        POINTER_SIZE,
    )
    .expect("workload must have an initialized typed main constant");
    (graph, root)
}
pub fn canonical(source: &str) -> (Graph, Box<[Data]>) {
    let (graph, root) = build(&mut parse(source));
    canonicalizer::process_graph(&graph, [root])
}

pub struct Workload {
    pub name: String,
    pub source: String,
    pub full_pipeline: bool,
}
pub fn workloads() -> Vec<Workload> {
    let mut workloads = vec![];
    for (name, source, full_pipeline) in [
        (
            "fibonacci",
            include_str!("../../graph_builder/examples/test.rx"),
            false,
        ),
        (
            "sum_loop",
            include_str!("../../graph_builder/examples/sum_loop.rx"),
            false,
        ),
        (
            "redundant_arithmetic",
            include_str!("../../graph_builder/examples/redundant_arithmetic.rx"),
            false,
        ),
        (
            "boolean_merges",
            include_str!("../../graph_builder/examples/boolean_merges.rx"),
            true,
        ),
        (
            "boolean_loop",
            include_str!("../../graph_builder/examples/boolean_loop.rx"),
            true,
        ),
    ] {
        workloads.push(Workload {
            name: name.into(),
            source: source.into(),
            full_pipeline,
        });
    }
    for size in [16, 64, 256] {
        workloads.push(Workload {
            name: format!("boolean_chain_{size}"),
            source: boolean_chain(size),
            full_pipeline: true,
        });
        workloads.push(Workload {
            name: format!("arithmetic_chain_{size}"),
            source: arithmetic_chain(size),
            full_pipeline: false,
        });
    }
    workloads
}

/// A dependency chain kept live by returning its last binding.
pub fn boolean_chain(size: usize) -> String {
    let mut source = String::from("let main bool = {\n    let v0 = true\n");
    for i in 1..=size {
        writeln!(source, "    let v{i} = !(v{} || false)", i - 1).unwrap();
    }
    writeln!(source, "    v{size}\n}}").unwrap();
    source
}
pub fn arithmetic_chain(size: usize) -> String {
    let mut source = String::from("let main usize = {\n    let v0 = 1\n");
    for i in 1..=size {
        writeln!(source, "    let v{i} = ((v{} + 0) * 1) % 1024", i - 1).unwrap();
    }
    writeln!(source, "    v{size}\n}}").unwrap();
    source
}

/// Integer source lowering is incomplete; these isolate existing peephole rules.
/// Values stay bounded, avoiding overflow even in debug builds.
pub fn integer_graph(size: usize, foldable: bool) -> (Graph, Data) {
    let mut graph = Graph::default();
    let one = graph.add(DataKind::Literal(1));
    let mut value = one;
    if !foldable {
        value = graph.add(DataKind::Unary {
            op: canonicalizer::UnaryOp::Neg,
            value,
        });
    }
    for _ in 0..size {
        value = graph.add(DataKind::ComBinary {
            op: ComBinaryOp::Add,
            ops: OrderedOps::sorted([value, one]),
        });
    }
    (graph, value)
}
