use std::path::Path;

use aadcr::unpacker::webpack::jsonp::get_modules_form_jsonp;
use oxc_allocator::Allocator;
use oxc_parser::Parser;
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;

#[test]
fn webpack_jsonp() {
    let name = "tests/fixtures/jsonp.js";
    let path = Path::new(&name);
    let source_text = std::fs::read_to_string(path).expect("{name} not found");

    let allocator = Allocator::default();
    let source_type = SourceType::from_path(path).unwrap();

    let ret = Parser::new(&allocator, &source_text, source_type).parse();

    assert!(ret.errors.is_empty());

    let mut program = ret.program;

    let ret = SemanticBuilder::new(&source_text)
        .with_excess_capacity(2.0)
        .build(&program);

    assert!(ret.errors.is_empty());

    let unpack_result = get_modules_form_jsonp(&allocator, &mut program);

    assert!(unpack_result.is_some());

    let mut result = unpack_result.unwrap();

    assert_eq!(result.modules.len(), 2);

    insta::with_settings!({ prepend_module_to_snapshot => false, omit_expression => true }, {
      insta::assert_debug_snapshot!("webpack_jsonp", result
        .modules
        .iter_mut()
        .map(|m| {
            m.write_code();
            m
        })
        .collect::<Vec<_>>()
    );
    });
}