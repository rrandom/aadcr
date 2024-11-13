use oxc_ast::ast::{NumberBase, NumericLiteral};
use oxc_traverse::{Traverse, TraverseCtx};

use super::UnminifyPass;

/// Converts number literal to its decimal representation.
/// currently the comment node is not supported
pub struct UnNumericLiteral {
    changed: bool,
}

impl UnNumericLiteral {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl UnminifyPass<'_> for UnNumericLiteral {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnNumericLiteral {
    fn enter_numeric_literal(&mut self, node: &mut NumericLiteral<'a>, ctx: &mut TraverseCtx<'a>) {
        if !node.base.is_base_10() || format!("{}", node.value) != node.raw {
            *node = ctx.ast.numeric_literal(
                node.span,
                node.value,
                format!("{}", node.value),
                NumberBase::Decimal,
            );
            self.changed = true;
        }
    }
}

#[cfg(test)]
mod test {
    use crate::passes::tests::tester;
    use oxc_allocator::Allocator;

    fn run_test(source_text: &str, expected: &str) {
        let allocator = Allocator::default();

        let mut pass = super::UnNumericLiteral::new();
        tester(
            &allocator,
            "test_un_numeric_literal",
            source_text,
            expected,
            &mut pass,
        );
    }

    #[test]
    fn test_un_numeric_literal() {
        run_test(
            "65536;
123.4;
0b101010;
0o777;
-0x123;
4.2e2;
-2e4;",
            "65536;
123.4;
42/* 0b101010 */;
511/* 0o777 */;
-291/* -0x123 */;
420/* 4.2e2 */;
-20000/* -2e4 */;",
        );
    }
}
