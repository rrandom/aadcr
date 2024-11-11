use oxc_ast::ast::Expression;
use oxc_traverse::{Traverse, TraverseCtx};

use crate::unminify::UnminifyCtx;

use super::UnminifyPass;

pub struct UnBoolean<'a, 'ctx> {
    ctx: &'ctx UnminifyCtx<'a>,
    changed: bool,
}

impl<'a, 'ctx> UnBoolean<'a, 'ctx> {
    pub fn new(ctx: &'ctx UnminifyCtx<'a>) -> Self {
        Self {
            ctx,
            changed: false,
        }
    }
}

impl<'a> UnminifyPass<'a> for UnBoolean<'a, '_> {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnBoolean<'a, '_> {
    fn enter_expression(&mut self, node: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
        if let Expression::UnaryExpression(expr) = node
            && expr.operator.is_not()
            && let Expression::NumericLiteral(value) = &expr.argument
            && (value.raw == "0" || value.raw == "1")
        {
            *node = ctx
                .ast
                .expression_boolean_literal(expr.span, value.raw == "0");
            self.changed = true;
        }
    }
}

#[cfg(test)]
mod test {

    use super::UnminifyCtx;
    use oxc_allocator::Allocator;
    use oxc_span::SourceType;

    use crate::unminify::passes::tests::tester;

    fn run_test(source_text: &str, expected: &str) {
        let allocator = Allocator::default();
        let ctx = UnminifyCtx::new(source_text, &SourceType::mjs());

        let mut pass = super::UnBoolean::new(&ctx);
        tester(&allocator, source_text, expected, &mut pass);
    }

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
