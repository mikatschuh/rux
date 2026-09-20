use std::{
    fs::{self, File},
    io::{self, Read},
    process::Command,
};

use graph_builder::build_graph_debug;
use parser::{ParserOutput, parse};
use tokenizer::{Span, Tokenizer, TypeSize};

const POINTER_SIZE: TypeSize = 64;

#[derive(Debug)]
struct TokenizerDiagnostics {
    errors: Vec<(Span, tokenizer::Error)>,
}
impl tokenizer::Diagnostics for TokenizerDiagnostics {
    fn add(&mut self, span: Span, err: tokenizer::Error) {
        self.errors.push((span, err));
    }
}

#[derive(Debug)]
struct ParserDiagnostics {
    errors: Vec<(Span, parser::Error)>,
}
impl parser::Diagnostics for ParserDiagnostics {
    fn add(&mut self, span: Span, err: parser::Error) {
        self.errors.push((span, err));
    }
}

struct GraphBuilderDiagnostics {
    errors: Vec<(Span, graph_builder::Error)>,
}
impl graph_builder::Diagnostics for GraphBuilderDiagnostics {
    fn add(&mut self, span: Span, err: graph_builder::Error) {
        self.errors.push((span, err));
    }
}

fn main() -> Result<(), io::Error> {
    let mut file = File::open("graph_builder/examples/test.rx")?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;

    let tokenizer = Tokenizer::new(
        content.leak(),
        TokenizerDiagnostics { errors: vec![] },
        POINTER_SIZE,
    );
    let ParserOutput {
        ast,
        item_table,
        mut interner,
        tokenizer_errors,
        parser_errors,
        ..
    } = parse(tokenizer, ParserDiagnostics { errors: vec![] });

    let starting_point = interner.get("main");
    let (graph_dump, graph_builder_errors) = build_graph_debug(
        ast,
        item_table,
        interner,
        starting_point,
        GraphBuilderDiagnostics { errors: vec![] },
        POINTER_SIZE,
    );

    if !tokenizer_errors.errors.is_empty()
        || !parser_errors.errors.is_empty()
        || !graph_builder_errors.errors.is_empty()
    {
        println!(
            "{:?}\n{:?}\n{:?}",
            tokenizer_errors.errors, parser_errors.errors, graph_builder_errors.errors
        );
    } else {
        let path = std::env::temp_dir().join(format!("rux-graph-{}.html", std::process::id()));
        fs::write(&path, graph_dump)?;
        println!("Graph dump saved to {}", path.display());

        #[cfg(target_os = "macos")]
        let status = Command::new("open").arg(&path).status()?;
        #[cfg(target_os = "windows")]
        let status = Command::new("explorer.exe").arg(&path).status()?;
        #[cfg(not(any(target_os = "macos", target_os = "windows")))]
        let status = Command::new("xdg-open").arg(&path).status()?;

        if !status.success() {
            return Err(io::Error::other(format!(
                "Failed to open graph dump in the browser: {status}"
            )));
        }
    }

    Ok(())
}
