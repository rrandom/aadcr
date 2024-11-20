use super::UnminifyPass;
use oxc_allocator::{CloneIn, Vec};
use oxc_ast::ast::{Expression, Statement, UnaryOperator};
use oxc_span::SPAN;
use oxc_traverse::{Traverse, TraverseCtx};

pub struct UnAssignMerging {
    changed: bool,
}

impl UnAssignMerging {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl UnminifyPass<'_> for UnAssignMerging {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnAssignMerging {
    fn enter_statements(&mut self, stmts: &mut Vec<'a, Statement<'a>>, ctx: &mut TraverseCtx<'a>) {
        let mut i = 0;
        while i < stmts.len() {
            let current = stmts.get_mut(i).unwrap();
            if let Statement::ExpressionStatement(expr_stmt) = current {
                let insertion =
                    self.try_un_merging_in_assignment_expression(&mut expr_stmt.expression, ctx);
                if let Some(insertion) = insertion {
                    let len = insertion.len();
                    stmts.splice(i + 1..i + 1, insertion);
                    i += len;
                }
            }
            i += 1;
        }
    }
}

impl<'a> UnAssignMerging {
    fn try_un_merging_in_assignment_expression(
        &mut self,
        expr: &mut Expression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Vec<'a, Statement<'a>>> {
        if let Expression::AssignmentExpression(assign) = expr
            && let Expression::AssignmentExpression(assign2) = &mut assign.right
            && let Some(right_most) = Self::try_get_right_most_assignment_expression(assign2, ctx)
        {
            let mut it = &mut assign.right;
            let mut insertion = ctx.ast.vec();

            while let Some(rest) = self.un_merging_assignment_right_expression(
                it,
                right_most.clone_in(ctx.ast.allocator),
                ctx,
            ) {
                insertion.push(rest);
                let last = insertion.last_mut().unwrap(); // get from insertion to avoid lifetime issue

                let Expression::AssignmentExpression(rest) = last else {
                    unreachable!()
                };
                it = &mut rest.right;
            }
            return Some(
                ctx.ast.vec_from_iter(
                    insertion
                        .into_iter()
                        .map(|expr| ctx.ast.statement_expression(SPAN, expr)),
                ),
            );
        }

        None
    }

    fn try_get_right_most_assignment_expression<'b>(
        expr: &'b oxc_ast::ast::AssignmentExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) -> Option<Expression<'a>> {
        let mut n = expr;

        while let Expression::AssignmentExpression(right) = &n.right {
            n = right;
        }
        let right_most = &n.right;
        let is_simple_value = match right_most {
            Expression::Identifier(_)
            | Expression::NullLiteral(_)
            | Expression::StringLiteral(_)
            | Expression::NumericLiteral(_)
            | Expression::BooleanLiteral(_)
            | Expression::BigIntLiteral(_) => true,
            Expression::UnaryExpression(expr) if expr.operator == UnaryOperator::LogicalNot => true,
            expr if expr.is_undefined() || expr.is_void_0() => true,
            _ => false,
        };

        if is_simple_value {
            Some(right_most.clone_in(ctx.ast.allocator))
        } else {
            None
        }
    }

    fn un_merging_assignment_right_expression<'b>(
        &mut self,
        expr: &'b mut Expression<'a>,
        right_most: Expression<'a>,
        ctx: &'b TraverseCtx<'a>,
    ) -> Option<Expression<'a>> {
        let rest = ctx.ast.move_expression(expr);
        *expr = right_most.clone_in(ctx.ast.allocator);

        if matches!(rest, Expression::AssignmentExpression(_)) {
            return Some(rest);
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

        let mut pass = super::UnAssignMerging::new();
        tester(&allocator, source_text, expected, &mut pass);
    }

    #[test]
    fn test_un_assign_merging_of_chained_assignments_simple() {
        run_test(
            "
exports.foo = exports.bar = exports.baz = 1;
        ",
            "
exports.foo = 1;
exports.bar = 1;
exports.baz = 1;
        ",
        );
    }

    #[test]
    fn test_un_assign_merging_of_chained_assignments() {
        run_test(
            "
a1 = a2 = 0;
b1 = b2 = 0n;
c1 = c2 = '';
d1 = d2 = true;
e1 = e2 = null;
f1 = f2 = undefined;
g1 = g2 = foo;
        ",
            "
a1 = 0;
a2 = 0;
b1 = 0n;
b2 = 0n;
c1 = '';
c2 = '';
d1 = true;
d2 = true;
e1 = null;
e2 = null;
f1 = undefined;
f2 = undefined;
g1 = foo;
g2 = foo;
        ",
        );
    }

    #[test]
    fn test_un_assign_merging_of_chained_assignments_no_transform() {
        run_test(
            "
a1 = a2 = `template${foo}`;
b1 = b2 = Symbol();
c1 = c2 = /regex/;
d1 = d2 = foo.bar;
f1 = f2 = fn();
        ",
            "
a1 = a2 = `template${foo}`;
b1 = b2 = Symbol();
c1 = c2 = /regex/;
d1 = d2 = foo.bar;
f1 = f2 = fn();
        ",
        );
    }
}
