use oxc_allocator::CloneIn;
use oxc_ast::ast::{BinaryOperator, Expression, UnaryOperator};
use oxc_span::Span;
use oxc_traverse::{Traverse, TraverseCtx};

use super::UnminifyPass;

/// Converts minified `typeof` to its long form.
///  "typeof x < 'u'" => "typeof x !== 'undefined'"
///  "typeof x > 'u'" => "typeof x === 'undefined'"
pub struct UnTypeof {
    changed: bool,
}

impl UnTypeof {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl UnminifyPass<'_> for UnTypeof {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnTypeof {
    fn enter_binary_expression(
        &mut self,
        node: &mut oxc_ast::ast::BinaryExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let left = &node.left;
        let right = &node.right;
        match (left, right) {
            (Expression::UnaryExpression(unary), Expression::StringLiteral(right))
                if unary.operator == UnaryOperator::Typeof && right.value == "u" =>
            {
                match node.operator {
                    BinaryOperator::LessThan => {
                        *node = self.typoef_undefined(
                            node.span,
                            left,
                            BinaryOperator::StrictInequality,
                            ctx,
                        );
                        self.changed = true;
                    }
                    BinaryOperator::GreaterThan => {
                        *node = self.typoef_undefined(
                            node.span,
                            left,
                            BinaryOperator::StrictEquality,
                            ctx,
                        );
                        self.changed = true;
                    }
                    _ => {}
                }
            }
            (Expression::StringLiteral(left), Expression::UnaryExpression(unary))
                if unary.operator == UnaryOperator::Typeof && left.value == "u" =>
            {
                match node.operator {
                    BinaryOperator::LessThan => {
                        *node = self.typoef_undefined(
                            node.span,
                            right,
                            BinaryOperator::StrictEquality,
                            ctx,
                        );
                        self.changed = true;
                    }
                    BinaryOperator::GreaterThan => {
                        *node = self.typoef_undefined(
                            node.span,
                            right,
                            BinaryOperator::StrictInequality,
                            ctx,
                        );
                        self.changed = true;
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }
}

impl<'a> UnTypeof {
    fn typoef_undefined(
        &self,
        span: Span,
        left: &Expression<'a>,
        operator: BinaryOperator,
        ctx: &mut TraverseCtx<'a>,
    ) -> oxc_ast::ast::BinaryExpression<'a> {
        ctx.ast.binary_expression(
            span,
            left.clone_in(ctx.ast.allocator),
            operator,
            ctx.ast
                .expression_string_literal(Span::default(), "undefined"),
        )
    }
}

#[cfg(test)]
mod test {
    use crate::passes::tests::tester;
    use oxc_allocator::Allocator;

    fn run_test(source_text: &str, expected: &str) {
        let allocator = Allocator::default();

        let mut pass = super::UnTypeof::new();
        tester(
            &allocator,
            "test un_typeof",
            source_text,
            expected,
            &mut pass,
        );
    }

    #[test]
    fn test_un_typeof() {
        run_test(
            r#"
typeof x < "u";
"u" > typeof x;
typeof x > "u";
"#,
            r#"
typeof x !== "undefined";
typeof x !== "undefined";
typeof x === "undefined";
"#,
        );

        run_test(
            r#"
typeof x <= "u";
typeof x >= "u";
typeof x === "string";
typeof x === "number";
typeof x === "boolean";
typeof x === "symbol";
typeof x === "object";
typeof x === "bigint";
typeof x === "function";
typeof x === "undefined";
"#,
            r#"
typeof x <= "u";
typeof x >= "u";
typeof x === "string";
typeof x === "number";
typeof x === "boolean";
typeof x === "symbol";
typeof x === "object";
typeof x === "bigint";
typeof x === "function";
typeof x === "undefined";
"#,
        )
    }
}
