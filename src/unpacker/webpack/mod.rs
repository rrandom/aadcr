use jsonp::get_modules_form_jsonp;
use oxc_allocator::Allocator;
use oxc_ast::ast::{ObjectPropertyKind, Program, PropertyKey};

pub mod jsonp;
pub mod webpack4;
pub mod webpack5;

use super::{common::utils, UnpackResult};
use webpack4::get_modules_form_webpack4;
use webpack5::get_modules_form_webpack5;

pub fn get_modules_form_webpack<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
) -> UnpackResult<'a> {
    get_modules_form_webpack5(allocator, program)
        .or(get_modules_form_webpack4(allocator, program))
        .or(get_modules_form_jsonp(allocator, program))
}

use oxc_allocator::CloneIn;
use oxc_ast::ast::{Argument, Expression, IdentifierName, Statement};
use oxc_span::Atom;
use oxc_traverse::TraverseCtx;

pub trait RequireR<'a> {
    #[inline]
    fn is_esm(&self, expr: &Expression<'a>, _ctx: &TraverseCtx<'a>) -> bool {
        utils::is_esm_helper(expr)
    }
}

/// webpack4 version of `require.d`
pub trait RequireD4<'a> {
    /// if a call expression is `require.d`, return true, and add exports to module_exports
    /// ```js
    /// require.d(exports, "a", function() {
    ///     return moduleContent;
    /// })
    /// ```
    fn get_require_d<'b>(
        &self,
        expr: &'b Expression<'a>,
        _ctx: &TraverseCtx<'a>,
    ) -> Option<(Atom<'a>, &'b Expression<'a>)> {
        let Expression::CallExpression(call_expr) = expr else {
            return None;
        };
        let Expression::StaticMemberExpression(mem) = &call_expr.callee else {
            return None;
        };
        let (Expression::Identifier(idf), IdentifierName { name, .. }) =
            (&mem.object, &mem.property)
        else {
            return None;
        };
        if name.as_str() != "d" || idf.name != "require" {
            return None;
        };
        if call_expr.arguments.len() != 3 {
            return None;
        };
        let [Argument::Identifier(_), Argument::StringLiteral(name), Argument::FunctionExpression(f)] =
            call_expr.arguments.as_slice()
        else {
            return None;
        };
        let Some(body) = &f.body else { return None };
        if body.statements.len() != 1 {
            return None;
        };
        let Statement::ReturnStatement(ret) = &body.statements[0] else {
            return None;
        };
        let Some(arg) = &ret.argument else {
            return None;
        };

        Some((name.value.clone(), arg))
    }
}

/// webpack5 version of `require.d`
pub trait RequireD5<'a> {
    fn handle_export(&self, export_name: Atom<'a>, export_value: Expression<'a>);

    /// if a call expression is `require.d`, return true, and add exports to module_exports
    /// ```js
    /// require.d(exports, {
    ///   "default": getter,
    ///   [key]: getter
    /// })
    /// ```
    fn get_require_d<'b>(&self, expr: &'b Expression<'a>, ctx: &TraverseCtx<'a>) -> bool {
        let mut found = false;
        let Expression::CallExpression(call_expr) = expr else {
            return false;
        };
        let Expression::StaticMemberExpression(mem_expr) = &call_expr.callee else {
            return false;
        };
        let (Expression::Identifier(idr), IdentifierName { name, .. }) =
            (&mem_expr.object, &mem_expr.property)
        else {
            return false;
        };
        if name.as_str() != "d" || idr.name != "require" {
            return false;
        };
        let [Argument::Identifier(_), Argument::ObjectExpression(obj)] =
            call_expr.arguments.as_slice()
        else {
            return false;
        };
        if obj.properties.len() == 0 {
            return false;
        }
        for prop in obj.properties.iter() {
            let ObjectPropertyKind::ObjectProperty(obj_prop) = prop else {
                return false;
            };
            let export_name = match &obj_prop.key {
                PropertyKey::StringLiteral(s) => &s.value,
                PropertyKey::StaticIdentifier(s) => &s.name,
                _ => {
                    return false;
                }
            };

            let Some(fun_body) = utils::get_fun_body(obj_prop.value.without_parentheses()) else {
                // TO-DO
                // add a warning
                return false;
            };

            if fun_body.statements.len() == 1 {
                match &fun_body.statements[0] {
                    Statement::ReturnStatement(ret) => {
                        if let Some(arg) = &ret.argument {
                            self.handle_export(
                                export_name.clone(),
                                arg.without_parentheses().clone_in(ctx.ast.allocator),
                            );
                            found = true;
                        }
                    }
                    Statement::ExpressionStatement(expr) => {
                        let export_value = expr.expression.without_parentheses();
                        if matches!(
                            export_value,
                            Expression::Identifier(_) | Expression::StaticMemberExpression(_)
                        ) {
                            self.handle_export(
                                export_name.clone(),
                                export_value.clone_in(ctx.ast.allocator),
                            );
                            found = true;
                        }
                    }
                    _ => {}
                }
            } else {
                // TO-DO
                // add a warning
            }
        }
        found
    }
}
