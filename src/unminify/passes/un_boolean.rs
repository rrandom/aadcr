use oxc_ast::ast::{Expression, UnaryOperator};
use oxc_traverse::{Traverse, TraverseCtx};

use crate::unminify::UnminifyCtx;

pub struct UnBoolean<'a, 'ctx> {
    ctx: &'ctx UnminifyCtx<'a>,
}

impl<'a, 'ctx> UnBoolean<'a, 'ctx> {
    pub fn new(ctx: &'ctx UnminifyCtx<'a>) -> Self {
        Self { ctx }
    }
}

impl<'a> Traverse<'a> for UnBoolean<'a, '_> {
    fn enter_expression(&mut self, node: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
        if let Expression::UnaryExpression(expr) = node
            && expr.operator == UnaryOperator::LogicalNot
            && let Expression::NumericLiteral(value) = &expr.argument
            && (value.raw == "0" || value.raw == "1")
        {
            *node = ctx
                .ast
                .expression_boolean_literal(expr.span, value.raw == "0");
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::unminify::passes::tests::run_test;

    #[test]
    fn test_un_boolean() {
        run_test(
            "let a = !1;
const b = !0;

var obj = {
  value: !0
};",
            "let a = false;
const b = true;

var obj = {
  value: true
};",
        );
    }
}
