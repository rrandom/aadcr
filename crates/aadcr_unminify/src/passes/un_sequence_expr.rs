use oxc_allocator::{CloneIn, Vec};
use oxc_ast::ast::{Expression, ReturnStatement, Statement};
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
    // () => (a(), b(), c()) -> () => { a(); b(); return c() }
    fn enter_arrow_function_expression(
        &mut self,
        node: &mut oxc_ast::ast::ArrowFunctionExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        use oxc_ast::ast::AssignmentTarget::*;
        let statements = &mut node.body.statements;
        if statements.len() == 1 {
            let statement = statements.get_mut(0).unwrap();
            if let Statement::ExpressionStatement(expr) = statement
                && let Expression::SequenceExpression(expr) =
                    expr.expression.without_parentheses_mut()
            {
                let exprs = &mut expr.expressions;
                let len = exprs.len();
                let mut insertion = ctx.ast.vec();

                for expr in exprs.iter_mut().take(len - 1) {
                    insertion.push(
                        ctx.ast
                            .statement_expression(SPAN, ctx.ast.move_expression(expr)),
                    );
                }

                let last = exprs.last_mut().unwrap();

                match last {
                    Expression::AssignmentExpression(assign) => {
                        match &mut assign.left {
                            AssignmentTargetIdentifier(left) => {
                                let ret_statement = ctx.ast.statement_return(
                                    SPAN,
                                    Some(ctx.ast.expression_from_identifier_reference(
                                        left.clone_in(ctx.ast.allocator),
                                    )),
                                );
                                insertion.push(
                                    ctx.ast
                                        .statement_expression(SPAN, ctx.ast.move_expression(last)),
                                );

                                *statement = ret_statement;
                            }
                            StaticMemberExpression(left) => {
                                let ret_statement = ctx.ast.statement_return(
                                    SPAN,
                                    Some(ctx.ast.expression_member(
                                        ctx.ast.member_expression_from_static(
                                            left.clone_in(ctx.ast.allocator),
                                        ),
                                    )),
                                );
                                insertion.push(
                                    ctx.ast
                                        .statement_expression(SPAN, ctx.ast.move_expression(last)),
                                );
                                *statement = ret_statement;
                            }
                            _ => {}
                        };
                    }
                    _ => {
                        *statement = ctx
                            .ast
                            .statement_return(SPAN, Some(ctx.ast.move_expression(last)));
                    }
                }
                statements.splice(0..0, insertion);
                node.expression = false;
                self.changed = true;
            }
        }
    }

    fn exit_program(&mut self, node: &mut oxc_ast::ast::Program<'a>, ctx: &mut TraverseCtx<'a>) {
        self.unsequence_statements(&mut node.body, ctx);
    }
}

impl<'a> UnSequenceExpr {
    fn unsequence_statements(
        &mut self,
        statements: &mut Vec<'a, Statement<'a>>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let mut i = 0;
        while i < statements.len() {
            let statement = statements.get_mut(i).unwrap();

            match statement {
                Statement::ReturnStatement(ret) => {
                    if let Some(insertion) = self.try_un_sequence_return(ret, ctx) {
                        let len = insertion.len();
                        statements.splice(i..i, insertion);
                        i += len;
                    }
                }
                Statement::IfStatement(if_stmt) => {
                    if let Some(insertion) = self.try_un_sequence_sequce_expr(&mut if_stmt.test, ctx) {
                        let len = insertion.len();
                        statements.splice(i..i, insertion);
                        i += len;
                    }
                }
                _ => {}
            }
            i += 1;
        }
    }

    // TO-DO
    fn try_un_sequence_return(
        &mut self,
        ret: &mut ReturnStatement<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Vec<'a, Statement<'a>>> {
        if let Some(expr) = ret.argument.as_mut()
            && let Expression::SequenceExpression(expr) = expr.without_parentheses()
        {
            let exprs = expr.expressions.clone_in(ctx.ast.allocator);
            if let Some((last, exprs)) = exprs.split_last() {
                let mut replacement = ctx.ast.vec();

                for expr in exprs {
                    replacement.push(
                        ctx.ast
                            .statement_expression(SPAN, expr.clone_in(ctx.ast.allocator)),
                    );
                }
                ret.argument = Some(last.clone_in(ctx.ast.allocator));
                return Some(replacement);
            }
        }
        None
    }

    fn try_un_sequence_sequce_expr(
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
    fn test_un_sequence_expr_arrow_function() {
        run_test(
            r#"
            var foo = (m => (a(), b(), c))();
            var bar = (m => (m.a = 1, m.b = 2, m.c = 3))();"#,
            r#"
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
"#,
        );
    }

    #[test]
    #[ignore]
    fn test_un_sequence_expr_return() {
        run_test(
            r#"
            if(a) return b(), c();
else return d = 1, e = 2, f = 3;

return a(), b(), c()
"#,
            r#"
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
"#,
        );
    }

    #[test]
    fn test_un_sequence_expr_if() {
        run_test(
            r#"if (a(), b(), c()) {
  d()
}"#,
            r#"a();
b();

if (c()) {
  d();
}
"#,
        );
    }
}
