#![allow(clippy::print_stdout)]

use std::{env, path::Path};

use oxc_allocator::Allocator;
use oxc_parser::Parser;
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;
// use oxc_transformer::{EnvOptions, Targets, TransformOptions, Transformer};
use pico_args::Arguments;

pub use aadcr::unpacker::*;

fn main() {
    let args = Arguments::from_env();
    let name = env::args().nth(1).unwrap_or_else(|| "test.js".to_string());

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

    // println!("Original:\n");
    // println!("{source_text}\n");

    let mut program = ret.program;
    // let trivias = ret.trivias;

    let ret = SemanticBuilder::new(&source_text)
        // Estimate transformer will triple scopes, symbols, references
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
    let unpack_result = unpack(&allocator, &mut program, "tmp/output");
    // let Some(modules) = unpack_result.modules else {
    //     return;
    // };

    println!("result:{:?}", unpack_result.files.len());

    // for module in modules.iter() {
    //     println!(
    //         "module: {:?}, is_entry: {:?} ===",
    //         module.id, module.is_entry
    //     );

    //     let module_str = CodeGenerator::new().build(&module.content).code;
    //     println!("{module_str}");
    // }
}
