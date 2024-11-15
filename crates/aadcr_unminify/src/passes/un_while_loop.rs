use oxc_ast::ast::Statement;
use oxc_span::SPAN;
use oxc_traverse::{Traverse, TraverseCtx};

use crate::UnminifyCtx;

use super::UnminifyPass;

pub struct UnWhileLoop<'a, 'ctx> {
    ctx: &'ctx UnminifyCtx<'a>,
    changed: bool,
}

impl<'a, 'ctx> UnWhileLoop<'a, 'ctx> {
    pub fn new(ctx: &'ctx UnminifyCtx<'a>) -> Self {
        Self {
            ctx,
            changed: false,
        }
    }
}

impl<'a> UnminifyPass<'a> for UnWhileLoop<'a, '_> {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnWhileLoop<'a, '_> {
    fn enter_statement(&mut self, stmt: &mut Statement<'a>, ctx: &mut TraverseCtx<'a>) {
        if let Statement::ForStatement(for_loop) = stmt {
            if for_loop.init.is_none() && for_loop.update.is_none() {
                let test = &mut for_loop.test;
                let test = test
                    .take()
                    .unwrap_or(ctx.ast.expression_boolean_literal(SPAN, true));
                *stmt =
                    ctx.ast
                        .statement_while(SPAN, test, ctx.ast.move_statement(&mut for_loop.body));
                self.changed = true;
            }
        }
    }
}

#[cfg(test)]
mod test {

    use super::UnminifyCtx;
    use oxc_allocator::Allocator;
    use oxc_span::SourceType;

    use crate::passes::tests::tester;

    fn run_test(source_text: &str, expected: &str) {
        let allocator = Allocator::default();
        let ctx = UnminifyCtx::new(source_text, SourceType::mjs());

        let mut pass = super::UnWhileLoop::new(&ctx);
        tester(&allocator, source_text, expected, &mut pass);
    }

    #[test]
    fn test_un_while_loop() {
        run_test(
            "
for(;;) {
  console.log('hello')
}

for (; i < 10;) {
  console.log('hello')
}
",
            "
while (true) {
  console.log('hello')
}

while (i < 10) {
  console.log('hello')
}
",
        );
    }

    #[test]
    fn test_un_while_loop_for_with_init_or_update() {
        run_test(
            "            
for (let i = 0;;) {}

for (;; i++) {}",
            "
for (let i = 0;;) {}

for (;; i++) {}
",
        );
    }
}
