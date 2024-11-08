#![allow(clippy::print_stdout)]
use std::{env, path::Path};

pub use aadcr::unpacker::*;
use oxc_allocator::Allocator;
use oxc_parser::Parser;
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;
use pico_args::Arguments;

// const DEFAULT_FILE: &str = "test.js";
const DEFAULT_FILE: &str = "tests/fixtures/webpack5/dist/index.js";
// const DEFAULT_FILE: &str= "tests/fixtures/jsonp.js";
// const DEFAULT_FILE: &str = "tests/fixtures/browserify.js";

const DEFAULT_OUTPUT_DIR: &str = "tmp/output4";

fn main() {
    let mut args = Arguments::from_env();

    let name = env::args()
        .nth(1)
        .unwrap_or_else(|| DEFAULT_FILE.to_string());

    let output_dir: String = args
        .opt_value_from_str("--output")
        .unwrap()
        .unwrap_or_else(|| DEFAULT_OUTPUT_DIR.to_string());

    let path = Path::new(&name);
    let source_text = std::fs::read_to_string(path).expect("{name} not found");
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(path).unwrap();

    let ret = Parser::new(&allocator, &source_text, source_type).parse();

    if !ret.errors.is_empty() {
        println!("Parser Errors:");
        for error in ret.errors {
            let error = error.with_source_code(source_text.clone());
            println!("{error:?}");
        }
    }

    let mut program = ret.program;
    // let trivias = ret.trivias;

    let ret = SemanticBuilder::new(&source_text)
        .with_excess_capacity(2.0)
        .with_cfg(true)
        .build(&program);

    if !ret.errors.is_empty() {
        println!("Semantic Errors:");
        for error in ret.errors {
            let error = error.with_source_code(source_text.clone());
            println!("{error:?}");
        }
    }

    let result = Unpacker::new(&allocator, &mut program, &source_text).build(&output_dir);

    println!("write to {output_dir} with {} files", result.files.len());
    for file in result.files.iter() {
        println!("  {}", file.display());
    }
}
