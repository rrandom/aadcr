use oxc_allocator::FromIn;
use oxc_ast::ast::{Expression, TemplateElement, TemplateElementValue, TemplateLiteral};
use oxc_span::{Atom, SPAN};
use oxc_traverse::{Traverse, TraverseCtx};

use super::UnminifyPass;

///  Restore template literal syntax from string concatenation.
/// // TypeScript / Babel / SWC / esbuild
/// "the ".concat(first, " take the ").concat(second, " and ").concat(third);
/// ->
/// `the ${first} take the ${second} and ${third}`
pub struct UnTemplateLiteral {
    changed: bool,
}

impl UnTemplateLiteral {
    pub fn new() -> Self {
        Self { changed: false }
    }
}

impl UnminifyPass<'_> for UnTemplateLiteral {
    fn changed(&self) -> bool {
        self.changed
    }
}

impl<'a> Traverse<'a> for UnTemplateLiteral {
    fn exit_expression(&mut self, node: &mut Expression<'a>, ctx: &mut TraverseCtx<'a>) {
        if let Expression::CallExpression(call_expr) = node
            && let Expression::StaticMemberExpression(member_expr) = &mut call_expr.callee
            && matches!(
                member_expr.object,
                Expression::StringLiteral(_) | Expression::TemplateLiteral(_)
            )
            && member_expr.property.name == "concat"
        {
            let mut template = create_empty_template(ctx);

            push_to_template(&mut template, &mut member_expr.object, ctx);

            for i in call_expr.arguments.iter_mut() {
                push_to_template(&mut template, i.as_expression_mut().unwrap(), ctx);
            }

            *node = ctx.ast.expression_from_template_literal(template);
        }
    }
}

fn create_empty_template<'a>(ctx: &mut TraverseCtx<'a>) -> TemplateLiteral<'a> {
    let quasis = ctx.ast.vec1(create_empty_quasi(ctx, true));
    let exprs = ctx.ast.vec();
    ctx.ast.template_literal(SPAN, quasis, exprs)
}

fn create_empty_quasi<'a>(ctx: &mut TraverseCtx<'a>, tail: bool) -> TemplateElement<'a> {
    let value = TemplateElementValue {
        raw: "".into(),
        cooked: None,
    };
    ctx.ast.template_element(SPAN, tail, value)
}

fn push_to_template<'a>(
    template: &mut TemplateLiteral<'a>,
    expr: &mut Expression<'a>,
    ctx: &mut TraverseCtx<'a>,
) {
    match expr {
        Expression::StringLiteral(s) => {
            let last_quasi = template.quasis.last_mut().unwrap();
            last_quasi.value.raw = Atom::from_in(
                &format!("{}{}", last_quasi.value.raw, s.value),
                ctx.ast.allocator,
            );
        }
        Expression::TemplateLiteral(t) => {
            let last_quasi = template.quasis.last_mut().unwrap();

            let rest = t.quasis.split_off(1).into_iter();

            let first = t.quasis.first().unwrap();

            last_quasi.value.raw = Atom::from_in(
                &format!("{}{}", last_quasi.value.raw, first.value.raw),
                ctx.ast.allocator,
            );

            let mut rest_vec = ctx.ast.vec_from_iter(rest);

            template.quasis.extend(ctx.ast.move_vec(&mut rest_vec));
            template
                .expressions
                .extend(ctx.ast.move_vec(&mut t.expressions));
        }
        _ => {
            template.expressions.push(ctx.ast.move_expression(expr));
            template.quasis.push(create_empty_quasi(ctx, false));
        }
    }
}

#[cfg(test)]
mod test {
    use crate::passes::tests::tester;
    use oxc_allocator::Allocator;

    fn run_test(source_text: &str, expected: &str) {
        let allocator = Allocator::default();

        let mut pass = super::UnTemplateLiteral::new();
        tester(&allocator, source_text, expected, &mut pass);
    }

    #[test]
    fn test_un_template_literal_restore() {
        run_test(
            r#"
var example1 = "the ".concat("simple ", form);
var example2 = "".concat(1);
var example3 = 1 + "".concat(foo).concat(bar).concat(baz);
var example4 = 1 + "".concat(foo, "bar").concat(baz);
var example5 = "".concat(1, f, "oo", true).concat(b, "ar", 0).concat(baz);
var example6 = "test ".concat(foo, " ").concat(bar);
"#,
            "
var example1 = `the simple ${form}`;
var example2 = `${1}`;
var example3 = 1 + `${foo}${bar}${baz}`;
var example4 = 1 + `${foo}bar${baz}`;
var example5 = `${1}${f}oo${true}${b}ar${0}${baz}`;
var example6 = `test ${foo} ${bar}`;
            ",
        );
    }

    #[test]
    fn test_un_template_literal_multiple() {
        run_test(
            r#"
"the ".concat(first, " take the ").concat(second, " and ").concat(third);
"#,
            "
`the ${first} take the ${second} and ${third}`
            ",
        );
    }
}
