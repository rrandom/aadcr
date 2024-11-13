use oxc_traverse::Traverse;

pub mod un_boolean;
pub mod un_infinity;
pub mod un_numeric_literal;
pub mod un_sequence_expr;
pub mod un_typeof;
pub mod un_undefined;
pub trait UnminifyPass<'a>: Traverse<'a> {
    fn changed(&self) -> bool;
}

#[cfg(test)]
pub mod tests {
    use super::UnminifyPass;
    use oxc_allocator::Allocator;
    use oxc_codegen::{CodeGenerator, CodegenOptions};
    use oxc_parser::Parser;
    use oxc_semantic::SemanticBuilder;
    use oxc_span::SourceType;
    use oxc_traverse::TraverseCtx;

    pub fn tester<'a, P: UnminifyPass<'a>>(
        allocator: &'a Allocator,
        name: &str,
        source_text: &'a str,
        expected: &'a str,
        pass: &mut P,
    ) {
        let result = run(allocator, source_text, Some(pass));
        let expected = run::<P>(allocator, expected, None);
        assert_eq!(
            result, expected,
            "\n{name}\nfor source:\n{source_text}\nexpect:\n{expected}\ngot:\n{result}"
        );
    }

    fn run<'a, P: UnminifyPass<'a>>(
        allocator: &'a Allocator,
        source_text: &'a str,
        pass: Option<&mut P>,
    ) -> String {
        let source_type = SourceType::mjs();
        let mut program = Parser::new(allocator, source_text, source_type)
            .parse()
            .program;

        if let Some(pass) = pass {
            let (symbols, scopes) = SemanticBuilder::new("")
                .build(&program)
                .semantic
                .into_symbol_table_and_scope_tree();
            let mut ctx = TraverseCtx::new(scopes, symbols, allocator);
            oxc_traverse::walk_program(pass, &mut program, &mut ctx);
        }

        CodeGenerator::new()
            .with_options(CodegenOptions {
                single_quote: true,
                ..CodegenOptions::default()
            })
            .build(&program)
            .code
    }
}
