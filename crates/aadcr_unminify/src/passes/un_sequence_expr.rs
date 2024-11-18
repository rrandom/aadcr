use oxc_allocator::{CloneIn, Vec};
use oxc_ast::ast::{AssignmentTarget, Expression, Statement};
use oxc_span::SPAN;
use oxc_traverse::{Traverse, TraverseCtx};

use super::UnminifyPass;

trait ExpressionMut {
    fn without_parentheses_mut(&mut self) -> &mut Self;
}

impl ExpressionMut for Expression<'_> {
    fn without_parentheses_mut(&mut self) -> &mut Self {
        let mut expr = self;
        while let Expression::ParenthesizedExpression(paran_expr) = expr {
            expr = &mut paran_expr.expression;
        }
        expr
    }
}

/// Separate sequence expressions into multiple statements.
///  `a(), b(), c()` -> `a(); b(); c();`
///  `return a(), b()` -> `a(); return b()`
pub struct UnSequenceExpr {
    changed: bool,
}

impl UnSequenceExpr {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl UnminifyPass<'_> for UnSequenceExpr {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnSequenceExpr {
    fn enter_arrow_function_expression(
        &mut self,
        node: &mut oxc_ast::ast::ArrowFunctionExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let stmts = &mut node.body.statements;
        if stmts.len() == 1 {
            let stmt = stmts.get_mut(0).unwrap();
            if let Statement::ExpressionStatement(expr) = stmt {
                let expr = &mut expr.expression;

                if let Some(mut insertion) = self.try_un_seq_seq_expr(expr, ctx) {
                    if let Some(ret) = self.try_un_sequence_return_assign(expr, ctx) {
                        insertion.push(ret);
                    }
                    *stmt = ctx
                        .ast
                        .statement_return(SPAN, Some(ctx.ast.move_expression(expr)));
                    stmts.splice(0..0, insertion);
                }

                node.expression = false;
                self.changed = true;
            }
        }
    }

    fn enter_variable_declaration(
        &mut self,
        node: &mut oxc_ast::ast::VariableDeclaration<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        // for decl in node.declarations.iter_mut() {
        //     if let Some(init) = &mut decl.init {
        //         if let Some(mut insertion) = self.try_un_seq_seq_expr(init, ctx) {
        //             let len = insertion.len();
        //         }
        //     }
        // }
    }

    fn exit_statements(&mut self, stmts: &mut Vec<'a, Statement<'a>>, ctx: &mut TraverseCtx<'a>) {
        let mut i = 0;
        while i < stmts.len() {
            let current = stmts.get_mut(i).unwrap();

            match current {
                Statement::ExpressionStatement(expr) => {
                    if let Some(insertion) = self.try_un_seq_seq_expr(&mut expr.expression, ctx) {
                        let len = insertion.len();
                        stmts.splice(i..i, insertion);
                        i += len;
                    }
                }
                Statement::ReturnStatement(ret) => {
                    if let Some(expr) = ret.argument.as_mut() {
                        if let Some(insertion) = self.try_un_seq_seq_expr(expr, ctx) {
                            let len = insertion.len();
                            stmts.splice(i..i, insertion);
                            i += len;
                        }
                    }
                }
                Statement::IfStatement(if_stmt) => {
                    self.un_seq_stmt_to_block(&mut if_stmt.consequent, ctx);

                    if let Some(alternate) = if_stmt.alternate.as_mut() {
                        self.un_seq_stmt_to_block(alternate, ctx);
                    }

                    if let Some(test_insertion) = self.try_un_seq_seq_expr(&mut if_stmt.test, ctx) {
                        let len = test_insertion.len();
                        stmts.splice(i..i, test_insertion);
                        i += len;
                    }
                }
                Statement::SwitchStatement(switch_stmt) => {
                    if let Some(insertion) =
                        self.try_un_seq_seq_expr(&mut switch_stmt.discriminant, ctx)
                    {
                        let len = insertion.len();
                        stmts.splice(i..i, insertion);
                        i += len;
                    }
                }
                Statement::VariableDeclaration(var_decl) => {
                    let mut insertion = ctx.ast.vec();
                    let mut length = 0;
                    for decl in var_decl.declarations.iter_mut() {
                        if let Some(init) = &mut decl.init
                            && let Some(insert) = self.try_un_seq_seq_expr(init, ctx)
                        {
                            length += insert.len();
                            insertion.extend(insert);
                        }
                    }
                    stmts.splice(i..i, insertion);
                    i += length;
                }
                _ => {}
            }
            i += 1;
        }
    }
}

impl<'a> UnSequenceExpr {
    fn try_un_seq_seq_expr(
        &mut self,
        expr: &mut Expression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Vec<'a, Statement<'a>>> {
        if let Expression::SequenceExpression(seq_expr) = expr.without_parentheses_mut() {
            let exprs = &mut seq_expr.expressions;
            let mut insertion = ctx.ast.vec();
            let len = exprs.len();

            for expr in exprs.iter_mut().take(len - 1) {
                insertion.push(
                    ctx.ast
                        .statement_expression(SPAN, ctx.ast.move_expression(expr)),
                );
            }
            let last = exprs.last_mut().unwrap();
            *expr = ctx.ast.move_expression(last);
            return Some(insertion);
        }
        None
    }

    fn un_seq_stmt_to_block(&mut self, stmt: &mut Statement<'a>, ctx: &mut TraverseCtx<'a>) {
        match stmt {
            Statement::ExpressionStatement(expr) => {
                if let Some(mut insertion) = self.try_un_seq_seq_expr(&mut expr.expression, ctx) {
                    insertion.push(ctx.ast.move_statement(stmt));
                    *stmt = ctx.ast.statement_block(SPAN, insertion);
                }
            }
            Statement::ReturnStatement(ret) => {
                if let Some(argument) = ret.argument.as_mut()
                    && let Some(mut insertion) = self.try_un_seq_seq_expr(argument, ctx)
                {
                    if let Some(mut st) = self.try_un_sequence_return_assign(argument, ctx) {
                        insertion.push(ctx.ast.move_statement(&mut st));
                    }
                    insertion.push(ctx.ast.move_statement(stmt));
                    *stmt = ctx.ast.statement_block(SPAN, insertion);
                }
            }
            Statement::ThrowStatement(throw) => {
                if let Some(mut insertion) = self.try_un_seq_seq_expr(&mut throw.argument, ctx) {
                    insertion.push(ctx.ast.move_statement(stmt));
                    *stmt = ctx.ast.statement_block(SPAN, insertion);
                }
            }
            _ => {}
        }
    }

    // `return m.c = 3` => `m.c = 3; return m.c`
    fn try_un_sequence_return_assign(
        &mut self,
        argument: &mut Expression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Statement<'a>> {
        match argument {
            Expression::AssignmentExpression(assign) => match &mut assign.left {
                AssignmentTarget::AssignmentTargetIdentifier(left) => {
                    let new_arg = ctx
                        .ast
                        .expression_from_identifier_reference(left.clone_in(ctx.ast.allocator));
                    let ret = Some(
                        ctx.ast
                            .statement_expression(SPAN, ctx.ast.move_expression(argument)),
                    );
                    *argument = new_arg;
                    ret
                }
                AssignmentTarget::StaticMemberExpression(left) => {
                    let new_arg = ctx.ast.expression_member(
                        ctx.ast
                            .member_expression_from_static(left.clone_in(ctx.ast.allocator)),
                    );
                    let ret = Some(
                        ctx.ast
                            .statement_expression(SPAN, ctx.ast.move_expression(argument)),
                    );
                    *argument = new_arg;
                    ret
                }
                _ => None,
            },
            _ => None,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::passes::tests::tester;
    use oxc_allocator::Allocator;

    fn run_test(source_text: &str, expected: &str) {
        let allocator = Allocator::default();

        let mut pass = super::UnSequenceExpr::new();
        tester(&allocator, source_text, expected, &mut pass);
    }

    #[test]
    fn test_un_sequence_expr() {
        run_test("a(), b(), c()", "a(); b(); c();");
    }

    #[test]
    fn test_un_sequence_expr_return() {
        run_test("return a(), b()", "a(); return b();");
        run_test("return (a()), b()", "a(); return b();");
    }

    #[test]
    fn test_un_sequence_expr_return2() {
        run_test(
            "
if(a) return b(), c();
else return d = 1, e = 2, f = 3;

return a(), b(), c()
            ",
            "
if (a) {
  b();
  return c();
} else {
  d = 1;
  e = 2;
  f = 3;
  return f;
}

a();
b();
return c();
",
        );

        run_test(
            "
if(a) return b(), c();
else return m.a = 1, m.b = 2, m.c = 3;        
        ",
            "
if (a) {
  b();
  return c();
} else {
  m.a = 1;
  m.b = 2;
  m.c = 3;
  return m.c;
}
        ",
        );
    }

    #[test]
    fn test_un_sequence_expr_arrow_fn() {
        run_test(
            "
var foo = (m => (a(), b(), c))();
var bar = (m => (m.a = 1, m.b = 2, m.c = 3))();
        ",
            "
var foo = (m => {
  a();
  b();
  return c;
})();
var bar = (m => {
  m.a = 1;
  m.b = 2;
  m.c = 3;
  return m.c;
})();
        ",
        );
    }

    #[test]
    fn test_un_sequence_expr_if() {
        run_test(
            "
if (condition) a(), b();
else c(), d();
",
            "
if (condition) {
  a();
  b();
} else {
  c();
  d();
}",
        );

        run_test(
            "
if (a(), b(), c()) {
  d(), e()
}
          ",
            "
a();
b();

if (c()) {
  d();
  e();
}",
        )
    }

    #[test]
    fn test_un_sequence_not_in_while() {
        run_test(
            "
while (a(), b(), c()) {
  d(), e()
}
  ",
            "
while (a(), b(), c()) {
  d();
  e();
}
  ",
        );
    }

    #[test]
    fn test_un_sequence_not_in_do_while() {
        run_test(
            "
do {
  d(), e()
} while (a(), b(), c())
 ",
            "
do {
  d();
  e();
} while (a(), b(), c())
 ",
        )
    }

    #[test]
    fn test_un_sequence_expr_switch() {
        run_test(
            "
switch (a(), b(), c()) {
  case 1:
    d(), e()
}
    ",
            "
a();
b();

switch (c()) {
case 1:
  d();
  e();
}",
        );
    }

    #[test]
    fn test_un_sequence_not_in_ternary() {
        run_test(
            "
        condition ? (a(), b()) : c()
        ",
            "
        condition ? (a(), b()) : c()
        ",
        );
    }

    #[test]
    fn test_un_sequence_in_try_catch() {
        run_test(
            "
try {
  a(), b()
} catch (e) {
  c(), d()
}
",
            "
try {
  a();
  b();
} catch (e) {
  c();
  d();
}
",
        );
    }

    #[test]
    fn test_un_sequence_expr_throw() {
        run_test(
            "
if(e !== null) throw a(), e
        ",
            "
        if (e !== null) {
  a();
  throw e;
}
",
        )
    }

    #[test]
    fn test_un_sequence_expr_var_declaration() {
        run_test(
            "
const x = (a(), b(), c())
            ",
            "
a();
b();
const x = c();
            ",
        );

        run_test(
            "
const x = (a(), b()), y = 1, z = (c(), d())
            ",
            "
a(); c(); const x = b(), y = 1, z = d()
            ",
        );
    }
}
