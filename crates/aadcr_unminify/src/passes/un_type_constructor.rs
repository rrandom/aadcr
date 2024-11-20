use oxc_ast::ast::{BinaryOperator, Expression, NumberBase, UnaryOperator};
use oxc_span::SPAN;
use oxc_traverse::{Traverse, TraverseCtx};

use super::UnminifyPass;

/// Restore type constructors from minified code.
/// +x; // Number(x)
/// x + ""; // String(x)
/// [,,,]; // Array(3)
/// Unsafe:
// BigInt: +1n will throw TypeError
// Symbol: Symbol('foo') + "" will throw TypeError
pub struct UnTypeConstructor {
    changed: bool,
}

impl UnTypeConstructor {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl UnminifyPass<'_> for UnTypeConstructor {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnTypeConstructor {
    fn enter_expression(&mut self, node: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
        let new_expr = match node {
            Expression::UnaryExpression(unary) => self.try_un_unary_plus(unary, ctx),

            Expression::BinaryExpression(binary) => self.try_un_binary_plus(binary, ctx),

            Expression::ArrayExpression(arr) => self.try_un_arr(arr, ctx),

            _ => None,
        };

        if let Some(new_node) = new_expr {
            self.changed = true;
            *node = new_node;
        }
    }
}

impl<'a> UnTypeConstructor {
    fn try_un_unary_plus(
        &mut self,
        node: &mut oxc_ast::ast::UnaryExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Expression<'a>> {
        if node.operator != UnaryOperator::UnaryPlus {
            return None;
        }

        if !matches!(&node.argument, Expression::Identifier(_)) {
            return None;
        }

        Some(create_number_call(node, ctx))
    }

    fn try_un_binary_plus(
        &mut self,
        node: &mut oxc_ast::ast::BinaryExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Expression<'a>> {
        if node.operator != BinaryOperator::Addition {
            return None;
        }

        let is_empty_string = matches!(
            &node.right,
            Expression::StringLiteral(lit) if lit.value.is_empty()
        );
        if !is_empty_string {
            return None;
        }

        let value = ctx.ast.move_expression(&mut node.left);

        if value.is_string_literal() {
            return Some(value);
        }

        Some(create_string_constructor_call(value, ctx))
    }

    fn try_un_arr(
        &mut self,
        node: &mut oxc_ast::ast::ArrayExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Expression<'a>> {
        let elements = &mut node.elements;

        if elements.is_empty() {
            return None;
        }

        if !elements.iter().all(|ele| ele.is_elision()) {
            return None;
        }

        Some(create_array_constructor_call(elements.len(), ctx))
    }
}

#[inline]
fn create_number_call<'a>(
    node: &mut oxc_ast::ast::UnaryExpression<'a>,
    ctx: &mut TraverseCtx<'a>,
) -> Expression<'a> {
    use oxc_allocator::Box;

    let value = ctx.ast.move_expression(&mut node.argument);
    let arg = ctx.ast.argument_expression(value);

    ctx.ast.expression_call(
        SPAN,
        ctx.ast.expression_identifier_reference(SPAN, "Number"),
        None::<Box<_>>,
        ctx.ast.vec1(arg),
        false,
    )
}

#[inline]
fn create_string_constructor_call<'a>(
    value: Expression<'a>,
    ctx: &mut TraverseCtx<'a>,
) -> Expression<'a> {
    use oxc_allocator::Box;

    let arg = ctx.ast.argument_expression(value);

    ctx.ast.expression_call(
        SPAN,
        ctx.ast.expression_identifier_reference(SPAN, "String"),
        None::<Box<_>>,
        ctx.ast.vec1(arg),
        false,
    )
}

#[inline]
fn create_array_constructor_call<'a>(length: usize, ctx: &mut TraverseCtx<'a>) -> Expression<'a> {
    use oxc_allocator::Box;

    let length_literal = ctx.ast.expression_numeric_literal(
        SPAN,
        length as f64,
        length.to_string(),
        NumberBase::Decimal,
    );

    let arg = ctx.ast.argument_expression(length_literal);

    ctx.ast.expression_call(
        SPAN,
        ctx.ast.expression_identifier_reference(SPAN, "Array"),
        None::<Box<_>>,
        ctx.ast.vec1(arg),
        false,
    )
}

#[cfg(test)]
mod test {
    use crate::passes::tests::tester;
    use oxc_allocator::Allocator;

    fn run_test(source_text: &str, expected: &str) {
        let allocator = Allocator::default();

        let mut pass = super::UnTypeConstructor::new();
        tester(&allocator, source_text, expected, &mut pass);
    }

    #[test]
    fn test_un_type_constructor_minified_code() {
        run_test(
            "
+x;
x + \"\";
[,,,];
            ",
            "
Number(x);
String(x);
Array(3);
            ",
        );
    }

    #[test]
    fn test_un_type_constructor_minified_code_2() {
        run_test(
            "
var a = 6 + +x;
var b = x + \"a\";
var c = 'long string' + x + '';
var d = x + 5 + '';
var e = x + '' + 5;
var f = 'str' + x + '' + 5 + '' + 6;
var g = 'str' + '';

function foo(numStr, result) {
    var num = +numStr;
    var arr = [,,,].fill(num + '').join(' + ');
    return `${result} = ${arr}`;
}

const emptyArr = [];
const oneArr = [,];
            ",
            "
var a = 6 + Number(x);
var b = x + \"a\";
var c = String('long string' + x);
var d = String(x + 5);
var e = String(x) + 5;
var f = String(String('str' + x) + 5) + 6;
var g = 'str';

function foo(numStr, result) {
    var num = Number(numStr);
    var arr = Array(3).fill(String(num)).join(' + ');
    return `${result} = ${arr}`;
}

const emptyArr = [];
const oneArr = Array(1);
            ",
        );
    }
}
