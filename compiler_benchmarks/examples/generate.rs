//! Print a deterministic source workload; redirect stdout to save it as .rx.
use compiler_benchmarks::{arithmetic_chain, boolean_chain};
fn main() {
    let mut args = std::env::args().skip(1);
    let kind = args
        .next()
        .expect("usage: generate <boolean|arithmetic> <size>");
    let size: usize = args
        .next()
        .expect("missing size")
        .parse()
        .expect("size must be an integer");
    let source = match kind.as_str() {
        "boolean" => boolean_chain(size),
        "arithmetic" => arithmetic_chain(size),
        _ => panic!("expected boolean or arithmetic"),
    };
    print!("{source}");
}
