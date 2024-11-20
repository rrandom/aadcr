use std::any::Any;

use oxc_ast::ast::{
    Argument, BinaryExpression, BinaryOperator, Expression, IdentifierName, IdentifierReference,
    NumberBase, TSTypeParameterInstantiation, UnaryOperator,
};
use oxc_span::SPAN;
use oxc_traverse::{Ancestor, Traverse, TraverseCtx};

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
        if args.len() == 2
            && let Some(first) = args.first().and_then(|a| a.as_expression())
            && (first.is_null() || first.is_undefined() || first.is_void_0())
            && let Some(second) = args.get_mut(1).and_then(|a| a.as_expression_mut())
        {
            return Some(second);
        }
        None
    }

    fn try_get_fn_apply_callee<'b>(
        &self,
        callee: &'b Expression<'a>,
    ) -> Option<&'b IdentifierReference<'a>> {
        match callee {
            Expression::StaticMemberExpression(mem) => {
                if let Expression::Identifier(ident) = &mem.object
                    && mem.property.name == "apply"
                {
                    return Some(ident);
                }
            }
            Expression::ComputedMemberExpression(mem) => {
                if let Expression::Identifier(ident) = &mem.object
                    && let Expression::Identifier(prop) = &mem.expression
                    && prop.name == "apply"
                {
                    return Some(ident);
                }
            }
            _ => {}
        }
        None
    }

    fn try_obj_apply(
        &mut self,
        node: &mut oxc_ast::ast::CallExpression<'a>,
        ctx: &mut TraverseCtx<'a>,
    ) {
        match &mut node.callee {
            Expression::StaticMemberExpression(member) => match &member.object {
                Expression::StaticMemberExpression(member2) => {
                    let args = &mut node.arguments;
                    if let Some((this_arg, arg)) = self.try_get_obj_apply_args(args) {
                        let obj = &member2.object;

                        match (this_arg, obj) {
                            (Expression::Identifier(ident), Expression::Identifier(ident2)) => {
                                if ident.name == ident2.name {
                                    let arg = ctx.ast.argument_spread_element(
                                        SPAN,
                                        ctx.ast.move_expression(arg),
                                    );

                                    node.arguments = ctx.ast.vec1(arg);

                                    node.callee = ctx.ast.move_expression(&mut member.object);
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }

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
        ",
            "
obj.fn(...someArray);
",
        );
    }
}
