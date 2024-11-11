use oxc_ast::ast::{BinaryOperator, Expression, UnaryOperator};
use oxc_traverse::{Traverse, TraverseCtx};

use super::UnminifyPass;

/// Converts `1 / 0` to `Infinity`.
pub struct UnInfinity {
    changed: bool,
}

impl UnInfinity {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl UnminifyPass<'_> for UnInfinity {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnInfinity {
    fn enter_expression(&mut self, node: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
        println!("{:?}", node);
        if let Expression::BinaryExpression(expr) = node
            && expr.operator == BinaryOperator::Division
            && let Expression::NumericLiteral(right) = expr.right.without_parentheses()
            && right.value == 0.0
        {
            let left = expr.left.without_parentheses();
            match left {
                Expression::NumericLiteral(left) if left.value == 1.0 => {
                    *node = ctx
                        .ast
                        .expression_identifier_reference(expr.span, "Infinity");
                    self.changed = true;
                }
                Expression::UnaryExpression(left)
                    if left.operator == UnaryOperator::UnaryNegation
                        && let Expression::NumericLiteral(left) =
                            left.argument.without_parentheses()
                        && left.value == 1.0 =>
                {
                    *node = ctx.ast.expression_unary(
                        expr.span,
                        UnaryOperator::UnaryNegation,
                        ctx.ast
                            .expression_identifier_reference(expr.span, "Infinity"),
                    );
                    self.changed = true;
                }
                _ => {}
            };
        }
    }
}

#[cfg(test)]
mod test {
    use crate::unminify::passes::tests::tester;
    use oxc_allocator::Allocator;

    fn run_test(source_text: &str, expected: &str) {
        let allocator = Allocator::default();

        let mut pass = super::UnInfinity::new();
        tester(&allocator, source_text, expected, &mut pass);
    }

    #[test]
    fn test_un_infinity() {
        run_test(
            "0 / 0;
1 / 0;
-1 / 0;
99 / 0;

'0' / 0;
'1' / 0;
'-1' / 0;
'99' / 0;

x / 0;

[0 / 0, 1 / 0]
",
            "
0 / 0;
Infinity;
-Infinity;
99 / 0;

'0' / 0;
'1' / 0;
'-1' / 0;
'99' / 0;

x / 0;

[0 / 0, Infinity]
",
        );
    }
}
