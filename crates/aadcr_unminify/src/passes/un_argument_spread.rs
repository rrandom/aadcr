use oxc_ast::ast::{Argument, Expression, IdentifierReference};
use oxc_span::SPAN;
use oxc_traverse::{Traverse, TraverseCtx};

use super::UnminifyPass;

/// Transform `fn.apply` calls to spread operator.
pub struct UnArgumentSpread {
    changed: bool,
}

impl UnArgumentSpread {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl UnminifyPass<'_> for UnArgumentSpread {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnArgumentSpread {
    fn enter_call_expression(
        &mut self,
        node: &mut oxc_ast::ast::CallExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        if let Some(ident) = self.try_get_fn_apply_callee(&node.callee)
            && let Some(arg) = self.try_get_spread_element(&mut node.arguments)
        {
            let arg = ctx
                .ast
                .argument_spread_element(SPAN, ctx.ast.move_expression(arg));

            node.arguments = ctx.ast.vec1(arg);
            node.callee = ctx
                .ast
                .expression_identifier_reference(SPAN, ident.name.clone());
            self.changed = true;
        } else {
            self.try_obj_apply(node, ctx);
        }
    }
}

impl<'a> UnArgumentSpread {
    fn try_get_spread_element<'b>(
        &self,
        args: &'b mut oxc_allocator::Vec<'a, Argument<'a>>,
    ) -> Option<&'b mut Expression<'a>> {
        if args.len() != 2 {
            return None;
        }

        let (first, rest) = args.split_first_mut()?;

        let first = first.as_expression()?;
        let second = rest.first_mut()?.as_expression_mut()?;

        if first.is_null() || first.is_undefined() || first.is_void_0() {
            return Some(second);
        }
        None
    }

    fn try_get_fn_apply_callee<'b>(
        &self,
        callee: &'b Expression<'a>,
    ) -> Option<&'b IdentifierReference<'a>> {
        match callee {
            Expression::StaticMemberExpression(mem)
                if matches!(&mem.object, Expression::Identifier(_))
                    && mem.property.name == "apply" =>
            {
                match &mem.object {
                    Expression::Identifier(ident) => Some(ident),
                    _ => None,
                }
            }

            Expression::ComputedMemberExpression(mem)
                if matches!(&mem.object, Expression::Identifier(_))
                    && matches!(&mem.expression, Expression::Identifier(prop) if prop.name == "apply") =>
            {
                match &mem.object {
                    Expression::Identifier(ident) => Some(ident),
                    _ => None,
                }
            }

            _ => None,
        }
    }

    fn try_obj_apply(
        &mut self,
        node: &mut oxc_ast::ast::CallExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        let member = match &mut node.callee {
            Expression::StaticMemberExpression(member) => &mut member.object,
            Expression::ComputedMemberExpression(member) => &mut member.object,
            _ => return,
        };

        let obj = match &member {
            Expression::StaticMemberExpression(member2) => &member2.object,
            Expression::ComputedMemberExpression(member2) => &member2.object,
            _ => return,
        };

        let Some((this_arg, arg)) = self.try_get_obj_apply_args(&mut node.arguments) else {
            return;
        };

        if !is_same_object(this_arg, obj) {
            return;
        }

        let spread_arg = ctx
            .ast
            .argument_spread_element(SPAN, ctx.ast.move_expression(arg));
        node.arguments = ctx.ast.vec1(spread_arg);
        node.callee = ctx.ast.move_expression(member);
    }

    /// Try to get the first and second arguments of the call expression.
    fn try_get_obj_apply_args<'b>(
        &self,
        args: &'b mut oxc_allocator::Vec<'a, Argument<'a>>,
    ) -> Option<(&'b Expression<'a>, &'b mut Expression<'a>)> {
        if args.len() != 2 {
            return None;
        }

        let (first, rest) = args.split_first_mut()?;
        let first = first.as_expression()?;
        let second = rest.first_mut()?.as_expression_mut()?;

        Some((first, second))
    }
}

fn expr_same_variant(a: &Expression, b: &Expression) -> bool {
    use std::mem::discriminant;
    discriminant(a) == discriminant(b)
}

fn is_same_object(a: &Expression, b: &Expression) -> bool {
    match (a, b) {
        (Expression::Identifier(ident1), Expression::Identifier(ident2)) => {
            ident1.name == ident2.name
        }
        _ if expr_same_variant(a, b) => print_expr(a) == print_expr(b),
        _ => false,
    }
}

fn print_expr(expr: &Expression) -> String {
    use oxc_codegen::Codegen;

    let mut p = Codegen::new();
    p.print_expression(expr);
    p.into_source_text()
}

#[cfg(test)]
mod test {
    use crate::passes::tests::tester;
    use oxc_allocator::Allocator;

    fn run_test(source_text: &str, expected: &str) {
        let allocator = Allocator::default();

        let mut pass = super::UnArgumentSpread::new();
        tester(&allocator, source_text, expected, &mut pass);
    }

    #[test]
    fn test_un_argument_spread_fn_apply() {
        run_test(
            "
fn.apply(undefined, someArray);
fn.apply(null, someArray);
",
            "
fn(...someArray);
fn(...someArray);
",
        )
    }

    #[test]
    fn test_un_argument_spread_not_transform() {
        run_test(
            "
fn.apply(obj, someArray);
fn.apply(this, someArray);
fn.apply({}, someArray);
        ",
            "
fn.apply(obj, someArray);
fn.apply(this, someArray);
fn.apply({}, someArray);
",
        );
    }

    #[test]
    fn test_un_argument_spread_obj_fn_call() {
        run_test(
            "
obj.fn.apply(obj, someArray);

class T {
  fn() {
    this.fn.apply(this, someArray);
  }
}
        ",
            "
obj.fn(...someArray);

class T {
  fn() {
    this.fn(...someArray);
  }
}
",
        );
    }

    #[test]
    fn test_un_argument_spread_obj_computed_fn_call() {
        run_test(
            "
obj[fn].apply(obj, someArray);
            ",
            "
obj[fn](...someArray);
            ",
        )
    }

    #[test]
    fn test_un_argument_spread_obj_fn_call_not_transform() {
        run_test(
            "
obj.fn.apply(otherObj, someArray);
obj.fn.apply(undefined, someArray);
obj.fn.apply(void 0, someArray);
obj.fn.apply(null, someArray);
obj.fn.apply(this, someArray);
obj.fn.apply({}, someArray);
            ",
            "
obj.fn.apply(otherObj, someArray);
obj.fn.apply(undefined, someArray);
obj.fn.apply(void 0, someArray);
obj.fn.apply(null, someArray);
obj.fn.apply(this, someArray);
obj.fn.apply({}, someArray);
            ",
        )
    }

    #[test]
    fn test_un_argument_spread_obj_fn_call_with_array() {
        run_test(
            "
obj.fn.apply(obj, [1, 2, 3]);
            ",
            "
obj.fn(...[1, 2, 3]);
            ",
        )
    }

    #[test]
    fn test_un_argument_spread_obj_fn_long_this_arg() {
        run_test(
            "
foo[bar+1].baz.fn.apply(foo[bar+1].baz, someArray);
            ",
            "
foo[bar+1].baz.fn(...someArray);
            ",
        )
    }

    #[test]
    fn test_un_argument_spread_obj_fn_literal_this_arg() {
        run_test(
            "
[].fn.apply([], someArray);
            ",
            "
[].fn(...someArray);
            ",
        )
    }

    #[test]
    fn test_un_argument_spread_not_transform_obj_fn_literal_this_arg() {
        run_test(
            "
[].fn.apply([1], someArray);
            ",
            "
[].fn.apply([1], someArray);
            ",
        )
    }
}
