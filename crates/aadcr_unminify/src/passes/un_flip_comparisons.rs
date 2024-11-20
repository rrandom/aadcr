use oxc_ast::ast::{BinaryExpression, BinaryOperator, Expression};
use oxc_traverse::{Traverse, TraverseCtx};

use super::UnminifyPass;

/// Flips comparisons that are in the form of "literal comes first"
/// to "literal comes second".
///
/// * `1 < a` -> `a > 1`
/// * `undefined === foo` -> `foo === undefined`
/// * `null !== bar` -> `bar !== null`
pub struct UnFlipComparisons {
    changed: bool,
}

impl UnFlipComparisons {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl UnminifyPass<'_> for UnFlipComparisons {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnFlipComparisons {
    fn exit_binary_expression(
        &mut self,
        node: &mut BinaryExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let BinaryExpression {
            left,
            operator,
            right,
            ..
        } = node;

        let is_left_valid = self.is_left_valid(left);
        let is_right_valid = self.is_right_valid(right);

        if is_left_valid && is_right_valid {
            match operator {
                BinaryOperator::Equality
                | BinaryOperator::StrictEquality
                | BinaryOperator::Inequality
                | BinaryOperator::StrictInequality => {
                    self.swap_binary_expression(left, right, ctx);
                }
                BinaryOperator::LessThan => {
                    node.operator = BinaryOperator::GreaterThan;
                    self.swap_binary_expression(left, right, ctx);
                }
                BinaryOperator::LessEqualThan => {
                    node.operator = BinaryOperator::GreaterEqualThan;
                    self.swap_binary_expression(left, right, ctx);
                }
                BinaryOperator::GreaterThan => {
                    node.operator = BinaryOperator::LessThan;
                    self.swap_binary_expression(left, right, ctx);
                }
                BinaryOperator::GreaterEqualThan => {
                    node.operator = BinaryOperator::LessEqualThan;
                    self.swap_binary_expression(left, right, ctx);
                }
                _ => {}
            }
        }
    }
}

impl<'a> UnFlipComparisons {
    fn swap_binary_expression(
        &mut self,
        left: &mut Expression<'a>,
        right: &mut Expression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let left_v = ctx.ast.move_expression(left);
        let right_v = ctx.ast.move_expression(right);
        *left = right_v;
        *right = left_v;
        self.changed = true;
    }

    fn is_left_valid(&self, left: &Expression<'a>) -> bool {
        if left.is_void_0() {
            return true;
        }
        match left {
            Expression::NullLiteral(_)
            | Expression::BooleanLiteral(_)
            | Expression::NumericLiteral(_)
            | Expression::StringLiteral(_) => true,
            Expression::Identifier(id) => is_common_value_identifier(&id.name),
            Expression::UnaryExpression(u) => match &u.argument {
                Expression::Identifier(id) => is_common_value_identifier(&id.name),
                _ => false,
            },
            Expression::TemplateLiteral(t) => t.expressions.is_empty(),
            _ => false,
        }
    }

    fn is_right_valid(&self, right: &Expression<'a>) -> bool {
        let right = match right {
            Expression::UnaryExpression(right) => &right.argument,
            _ => right,
        };
        match right {
            Expression::Identifier(_)
            | Expression::StaticMemberExpression(_)
            | Expression::ComputedMemberExpression(_)
            | Expression::CallExpression(_) => true,
            _ => false,
        }
    }
}
fn is_common_value_identifier(name: impl AsRef<str>) -> bool {
    name.as_ref() == "NaN" || name.as_ref() == "Infinity" || name.as_ref() == "undefined"
}

#[cfg(test)]
mod test {
    use crate::passes::tests::tester;
    use oxc_allocator::Allocator;

    fn run_test(source_text: &str, expected: &str) {
        let allocator = Allocator::default();

        let mut pass = super::UnFlipComparisons::new();
        tester(&allocator, source_text, expected, &mut pass);
    }

    #[test]
    fn test_un_flip_comparisons() {
        run_test(
            "
void 0 === foo;
undefined === foo;
null !== foo;
1 == foo;
true != foo;
\"str\" == foo;
`test` == foo;
NaN == foo;
Infinity == foo;
-Infinity == foo;
\"function\" == typeof foo;

1 < bar;
1 > bar;
1 <= bar;
1 >= bar;
        ",
            "
foo === void 0;
foo === undefined;
foo !== null;
foo == 1;
foo != true;
foo == \"str\";
foo == `test`;
foo == NaN;
foo == Infinity;
foo == -Infinity;
typeof foo == \"function\";

bar > 1;
bar < 1;
bar >= 1;
bar <= 1;
        ",
        );

        run_test(
            "
        1 == obj.props;
1 == obj.props[0];
1 == method();
",
            "
obj.props == 1;
obj.props[0] == 1;
method() == 1;
",
        );

        run_test(
            "
2 === foo ? bar : baz;
",
            "
foo === 2 ? bar : baz;
",
        );

        run_test(
            "
foo === undefined;
foo !== null;
foo == 1;
foo != true;
foo == \"str\";
foo == `test`;
foo == `test${1}`;
foo == NaN;
foo == Infinity;
typeof foo == \"function\";

({} == foo);
`test${1}` == foo;

bar > 1;
bar < 1.2;
bar >= 1;
bar <= 1;
",
            "
foo === undefined;
foo !== null;
foo == 1;
foo != true;
foo == \"str\";
foo == `test`;
foo == `test${1}`;
foo == NaN;
foo == Infinity;
typeof foo == \"function\";

({} == foo);
`test${1}` == foo;

bar > 1;
bar < 1.2;
bar >= 1;
bar <= 1;
",
        );
    }
}
