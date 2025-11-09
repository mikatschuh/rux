use super::{BinaryOp, Graph, NodeId, NodeKind};
use crate::{error::Errors, parser::typing::TypeParser, tokenizing::Tokenizer, utilities::Rc};
use std::path::Path;

const EXAMPLE_SOURCE: &str = include_str!("example.rx");

fn tokenizer_for(source: &'static str) -> Tokenizer<'static> {
    let errors = Rc::new(Errors::empty(Path::new("example.rx")));
    Tokenizer::new(source, errors, TypeParser::new())
}

#[test]
fn builds_graph_for_example_program() {
    let mut tokenizer = tokenizer_for(EXAMPLE_SOURCE);
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    let kinds: Vec<_> = graph.nodes().iter().map(|node| node.kind.clone()).collect();

    assert!(matches!(
        kinds.as_slice(),
        [
            NodeKind::VariableDecl {
                name: "x",
                ty: "i32",
                initializer: None
            },
            NodeKind::Constant {
                literal: "10",
                value: 10
            },
            NodeKind::VariableDecl {
                name: "a",
                ty: "i32",
                initializer: Some(NodeId(1))
            },
            NodeKind::Read {
                name: "a",
                source: NodeId(1)
            },
            NodeKind::Constant {
                literal: "10",
                value: 10
            },
            NodeKind::Binary {
                op: BinaryOp::Add,
                lhs: NodeId(3),
                rhs: NodeId(4)
            },
            NodeKind::Constant {
                literal: "20",
                value: 20
            },
            NodeKind::Binary {
                op: BinaryOp::Mul,
                lhs: NodeId(5),
                rhs: NodeId(6)
            },
            NodeKind::Assign {
                name: "x",
                value: NodeId(7)
            },
            NodeKind::Constant {
                literal: "11",
                value: 11
            },
            NodeKind::VariableDecl {
                name: "y",
                ty: "i32",
                initializer: Some(NodeId(9))
            },
            NodeKind::VariableDecl {
                name: "a",
                ty: "u32",
                initializer: None
            },
        ]
    ));
}

#[test]
fn assignment_updates_variable_versions() {
    const PROGRAM: &str = "x i32 = 1\nx = x + 1\n";
    let errors = Rc::new(Errors::empty(Path::new("assign.rx")));
    let mut tokenizer = Tokenizer::new(PROGRAM, errors, TypeParser::new());
    let graph = Graph::from_stream(&mut tokenizer).expect("graph");

    assert_eq!(graph.nodes().len(), 6);
    assert!(matches!(
        graph.nodes()[0].kind,
        NodeKind::Constant {
            literal: "1",
            value: 1
        }
    ));
    assert!(matches!(
        graph.nodes()[1].kind,
        NodeKind::VariableDecl {
            name: "x",
            ty: "i32",
            initializer: Some(NodeId(0))
        }
    ));
    assert!(matches!(
        graph.nodes()[5].kind,
        NodeKind::Assign {
            name: "x",
            value: NodeId(4)
        }
    ));
    let symbol = graph.symbol("x").expect("x symbol");
    assert_eq!(symbol.last_value, NodeId(5));
    if let NodeKind::Binary { op, .. } = &graph.nodes()[4].kind {
        assert_eq!(*op, BinaryOp::Add);
    } else {
        panic!("expected addition node");
    }
}
