use oxc_ast::ast::Expression;
use oxc_traverse::{Traverse, TraverseCtx};

use super::UnminifyPass;

pub struct UnUndefined {
    changed: bool,
}

impl UnUndefined {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl UnminifyPass<'_> for UnUndefined {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnUndefined {
    fn enter_expression(&mut self, node: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
        if let Expression::UnaryExpression(expr) = node
            && expr.operator.is_void()
            && expr.argument.without_parentheses().is_number()
            && ctx.scopes().get_root_binding("undefined").is_none()
        {
            *node = ctx
                .ast
                .expression_identifier_reference(expr.span, "undefined");
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

        let mut pass = super::UnUndefined::new();
        tester(&allocator, source_text, expected, &mut pass);
    }

    #[test]
    fn test_un_undefined_void_0() {
        run_test(
            "if(void 0 !== a) {
            console.log('a')
          }
          ",
            "if(undefined !== a) {
            console.log('a')
          }
          ",
        );
        run_test(
            "void 0
        void 99
        void(0)",
            "undefined
        undefined
        undefined",
        );
    }

    #[test]
    fn test_un_undefined_void_function() {
        run_test(
            "void function() {
  console.log('a')
  return void a()
}",
            "void function() {
  console.log('a')
  return void a()
}",
        );
    }

    #[test]
    fn test_un_undefined_declared_in_scope() {
        run_test(
            "var undefined = 42;
console.log(void 0);

if (undefined !== a) {
  console.log('a', void 0);
}",
            "var undefined = 42;

console.log(void 0);

if (undefined !== a) {
  console.log('a', void 0);
}",
        );
    }
}
