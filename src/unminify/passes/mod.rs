use oxc_traverse::Traverse;

use super::UnminifyCtx;

pub mod un_boolean;

#[cfg(test)]
mod tests {

    use crate::unminify::UnminifyCtx;

    use super::*;

    use oxc_allocator::Allocator;
    use oxc_ast::ast::{Expression, UnaryOperator};
    use oxc_codegen::CodeGenerator;
    use oxc_parser::Parser;
    use oxc_semantic::SemanticBuilder;
    use oxc_span::SourceType;
    use oxc_traverse::{traverse_mut, Traverse, TraverseCtx};
    use un_boolean::UnBoolean;

    pub fn run_test<'a>(source: &str, expected: &str) {
        let allocator = Allocator::default();
        let source_type = SourceType::default();

        let ret = Parser::new(&allocator, &source, source_type).parse();

        assert!(ret.errors.is_empty());

        let mut program = ret.program;

        let ret = SemanticBuilder::new(&source)
            .with_excess_capacity(2.0)
            .build(&program);

        assert!(ret.errors.is_empty());

        let (symbols, scopes) = ret.semantic.into_symbol_table_and_scope_tree();

        let ctx = UnminifyCtx::new(&source, &source_type);
        let mut un_boolean = UnBoolean::new(&ctx);

        let (symbols, scopes) =
            traverse_mut(&mut un_boolean, &allocator, &mut program, symbols, scopes);

        let printed = CodeGenerator::new().build(&program).code;

        assert_eq!(
            printed.trim().replace("\n", ""),
            expected.trim().replace("\n", "")
        );
    }
}
