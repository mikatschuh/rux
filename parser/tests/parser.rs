use std::sync::{Arc, Mutex};

use parser::{Diagnostics, Error, ParserOutput};
use tokenizer::{Span, Tokenizer};

#[derive(Clone, Default)]
struct MockDiagnostics {
    parser_errors: Arc<Mutex<Vec<(Span, Error)>>>,
    tokenizer_errors: Arc<Mutex<Vec<(Span, tokenizer::Error)>>>,
}

impl Diagnostics for MockDiagnostics {
    fn add(&mut self, span: Span, err: Error) {
        self.parser_errors.lock().unwrap().push((span, err));
    }
}

impl tokenizer::Diagnostics for MockDiagnostics {
    fn add(&mut self, span: Span, err: tokenizer::Error) {
        self.tokenizer_errors.lock().unwrap().push((span, err));
    }
}

fn parse(source: &'static str) -> ParserOutput<MockDiagnostics, MockDiagnostics> {
    let errors = MockDiagnostics::default();
    let tokenizer = Tokenizer::new(source, errors.clone(), 64);
    parser::parse(tokenizer, errors)
}

#[test]
fn parses_top_level_let_item() {
    let mut output = parse("let main = 0");
    let main = output.interner.get("main");

    assert!(output.item_table.remove(&main).is_some());
    assert!(
        output
            .parser_errors
            .parser_errors
            .lock()
            .unwrap()
            .is_empty()
    );
    assert!(
        output
            .tokenizer_errors
            .tokenizer_errors
            .lock()
            .unwrap()
            .is_empty()
    );
}

#[test]
fn recovers_after_unexpected_top_level_token() {
    let mut output = parse("else\nlet main = 0");
    let main = output.interner.get("main");

    assert!(output.item_table.remove(&main).is_some());
    assert!(
        output
            .tokenizer_errors
            .tokenizer_errors
            .lock()
            .unwrap()
            .is_empty()
    );
    let errors = output.parser_errors.parser_errors.lock().unwrap();
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].0, Span::at(1, 1, 5, 1));
    assert!(matches!(&errors[0].1, Error::ExpectedItemDeclaration));
}

#[test]
fn consumes_empty_statements_inside_blocks() {
    let mut output = parse("let main = {; 0}");
    let main = output.interner.get("main");

    assert!(output.item_table.remove(&main).is_some());
    assert!(
        output
            .parser_errors
            .parser_errors
            .lock()
            .unwrap()
            .is_empty()
    );
    assert!(
        output
            .tokenizer_errors
            .tokenizer_errors
            .lock()
            .unwrap()
            .is_empty()
    );
}
