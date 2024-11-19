// crates/aadcr_unpacker/benches/unpacker_benchmark.rs

use aadcr_unpacker::*;
use browserify::get_modules_form_browserify;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use oxc_allocator::Allocator;
use oxc_parser::Parser;
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;
use webpack::webpack5::get_modules_form_webpack5;
use std::path::Path;

fn benchmark_browserify(c: &mut Criterion) {
    let name = "tests/fixtures/browserify.js";
    let path = Path::new(name);
    let source_text = std::fs::read_to_string(path).expect("{name} not found");
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(path).unwrap();
    
    c.bench_function("browserify_unpacker", |b| {
        b.iter(|| {
            let ret = Parser::new(&allocator, black_box(&source_text), source_type).parse();
        
            let program = ret.program;
        
            SemanticBuilder::new(&source_text)
                .with_excess_capacity(2.0)
                .build(&program);
            let _ = get_modules_form_browserify(&allocator, &program);
        })
    });
}

fn benchmark_webpack5(c: &mut Criterion) {
    let name = "tests/fixtures/webpack5/dist/index.js";
    let path = Path::new(name);
    let source_text = std::fs::read_to_string(path).expect("{name} not found");
    let allocator = Allocator::default();
    let source_type = SourceType::from_path(path).unwrap();
    
    c.bench_function("browserify_unpacker", |b| {
        b.iter(|| {
            let ret = Parser::new(&allocator, black_box(&source_text), source_type).parse();
        
            let program = ret.program;
        
            SemanticBuilder::new(&source_text)
                .with_excess_capacity(2.0)
                .build(&program);
            let _ = get_modules_form_webpack5(&allocator, &program,"");
        })
    });
}

criterion_group!(benches, benchmark_browserify, benchmark_webpack5);
criterion_main!(benches);
