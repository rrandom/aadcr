use oxc_allocator::CloneIn;
use oxc_ast::ast::{Expression, Statement};
use oxc_span::Span;
use oxc_traverse::{Traverse, TraverseCtx};


use super::UnminifyPass;

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
        let statements = &node.body.statements;
        if statements.len() == 1 {
            let statement = &statements[0];
            if let Statement::ExpressionStatement(expr) = statement
                && let Expression::SequenceExpression(expr) = expr.expression.without_parentheses()
            {
                let exprs = &expr.expressions;
                let mut body_statements = ctx.ast.vec_with_capacity(exprs.len() + 1);
                if let Some((last, exprs)) = exprs.split_last() {
                    for expr in exprs {
                        body_statements.push(ctx.ast.statement_expression(
                            Span::default(),
                            expr.clone_in(ctx.ast.allocator),
                        ));
                    }
                    if let Expression::AssignmentExpression(assign) = last {
                        match &assign.left {
                            AssignmentTargetIdentifier(left) => {
                                body_statements.push(ctx.ast.statement_expression(
                                    Span::default(),
                                    last.clone_in(ctx.ast.allocator),
                                ));

                                body_statements.push(ctx.ast.statement_return(
                                    Span::default(),
                                    Some(ctx.ast.expression_from_identifier_reference(
                                        left.clone_in(ctx.ast.allocator),
                                    )),
                                ));
                            }
                            StaticMemberExpression(left) => {
                                body_statements.push(ctx.ast.statement_expression(
                                    Span::default(),
                                    last.clone_in(ctx.ast.allocator),
                                ));
                                body_statements.push(ctx.ast.statement_return(
                                    Span::default(),
                                    Some(ctx.ast.expression_member(
                                        ctx.ast.member_expression_from_static(
                                            left.clone_in(ctx.ast.allocator),
                                        ),
                                    )),
                                ));
                            }
                            _ => {}
                        }
                    } else {
                        body_statements.push(ctx.ast.statement_return(
                            Span::default(),
                            Some(last.clone_in(ctx.ast.allocator)),
                        ))
                    }
                    node.body.statements = body_statements;
                    node.expression = false;
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::unminify::passes::tests::tester;
    use oxc_allocator::Allocator;

    fn run_test(name: &str, source_text: &str, expected: &str) {
        let allocator = Allocator::default();

        let mut pass = super::UnSequenceExpr::new();
        tester(&allocator, name, source_text, expected, &mut pass);
    }

    #[test]
    fn test_un_sequence_expr_arrow_function() {
        run_test(
            "test_un_sequence_expr_arrow_function",
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
}
