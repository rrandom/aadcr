use oxc_ast::ast::{
    BinaryExpression, BinaryOperator, Expression, NumberBase, TSTypeParameterInstantiation,
    UnaryOperator,
};
use oxc_span::SPAN;
use oxc_traverse::{Ancestor, Traverse, TraverseCtx};

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
        match node {
            Expression::UnaryExpression(unary) => {
                if let Some(call) = self.try_un_unary_plus(unary, ctx) {
                    self.changed = true;
                    *node = call
                }
            }
            Expression::BinaryExpression(binary) => {
                if let Some(call) = self.try_un_binary_plus(binary, ctx) {
                    self.changed = true;
                    *node = call
                }
            }
            Expression::ArrayExpression(arr) => {
                if let Some(call) = self.try_un_arr(arr, ctx) {
                    self.changed = true;
                    *node = call
                }
            }
            _ => {}
        }
    }
}

impl<'a> UnTypeConstructor {
    fn try_un_unary_plus(
        &mut self,
        node: &mut oxc_ast::ast::UnaryExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Expression<'a>> {
        use oxc_allocator::Box;

        if let Expression::Identifier(_) = &mut node.argument
            && node.operator == UnaryOperator::UnaryPlus
        {
            let value = ctx.ast.move_expression(&mut node.argument);
            let value = ctx.ast.argument_expression(value);

            let call = ctx.ast.expression_call(
                node.span,
                ctx.ast.expression_identifier_reference(SPAN, "Number"),
                None::<Box<_>>,
                ctx.ast.vec1(value),
                false,
            );
            return Some(call);
        }
        None
    }

    fn try_un_binary_plus(
        &mut self,
        node: &mut oxc_ast::ast::BinaryExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Expression<'a>> {
        use oxc_allocator::Box;

        if node.operator == BinaryOperator::Addition
            && let Expression::StringLiteral(lit) = &node.right
            && lit.value == ""
        {
            let value = ctx.ast.move_expression(&mut node.left);

            if value.is_string_literal() {
                return Some(value);
            }

            let value = ctx.ast.argument_expression(value);

            let call = ctx.ast.expression_call(
                node.span,
                ctx.ast.expression_identifier_reference(SPAN, "String"),
                None::<Box<_>>,
                ctx.ast.vec1(value),
                false,
            );
            return Some(call);
        }
        None
    }

    fn try_un_arr(
        &mut self,
        node: &mut oxc_ast::ast::ArrayExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Expression<'a>> {
        use oxc_allocator::Box;

        let elements = &mut node.elements;

        dbg!(&elements);

        let length = elements.len();
        if length > 0 && elements.iter().all(|ele| ele.is_elision()) {
            let length = ctx.ast.expression_numeric_literal(
                SPAN,
                length as f64,
                format!("{length}"),
                NumberBase::Decimal,
            );

            let value = ctx.ast.argument_expression(length);
            let call = ctx.ast.expression_call(
                node.span,
                ctx.ast.expression_identifier_reference(SPAN, "Array"),
                None::<Box<_>>,
                ctx.ast.vec1(value),
                false,
            );
            return Some(call);
        }
        None
    }
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
