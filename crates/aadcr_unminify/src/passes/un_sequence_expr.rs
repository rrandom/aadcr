use oxc_allocator::{CloneIn, Vec};
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
    

    fn exit_program(&mut self, node: &mut oxc_ast::ast::Program<'a>, ctx: &mut TraverseCtx<'a>) {
        self.unfuse_statements(&mut node.body, ctx);
    }
}

impl<'a> UnSequenceExpr {
    fn unfuse_statements(&mut self, statements: &mut Vec<'a, Statement<'a>>, ctx: &mut TraverseCtx<'a>) {
        let mut i = 0;
        while i < statements.len() {
            let mut replacement = ctx.ast.vec();

            match &mut statements.get_mut(i).unwrap() {
                Statement::ReturnStatement(ret) if let Some(expr) = ret.argument.as_ref()
                && let Expression::SequenceExpression(expr) = expr.without_parentheses() =>             {
                    let exprs = expr.expressions.clone_in(ctx.ast.allocator);
                    if let Some((last, exprs)) = exprs.split_last() {
                        for expr in exprs {
                            replacement.push(ctx.ast.statement_expression(
                                Span::default(),
                                expr.clone_in(ctx.ast.allocator),
                            ));
                        }
                        replacement.push(ctx.ast.statement_return(
                            Span::default(),
                            Some(last.clone_in(ctx.ast.allocator)),
                        ));
                        statements.splice(i..i+1, replacement);
                        i += exprs.len();
                        continue;
                    }
                }
                Statement::IfStatement(if_stmt) => {
                    if let Expression::SequenceExpression(expr) = if_stmt.test.clone_in(ctx.ast.allocator).without_parentheses() {
                        let exprs = expr.expressions.clone_in(ctx.ast.allocator);
                        if let Some((last, exprs)) = exprs.split_last() {
                            if_stmt.test = last.clone_in(ctx.ast.allocator);
                            for expr in exprs {
                                replacement.push(ctx.ast.statement_expression(Span::default(), expr.clone_in(ctx.ast.allocator)));
                            }
                            statements.splice(i..i, replacement);
                            i += exprs.len();
                            continue;
                        }
                    }
                }
                _ => {}
            }
            i += 1;
        }
    }
}

#[cfg(test)]
mod test {
    use crate::passes::tests::tester;
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

    #[test]
    #[ignore]
    fn test_un_sequence_expr_return() {
        run_test(
            "test_un_sequence_expr_return",
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
            "test_un_sequence_expr_if",
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
