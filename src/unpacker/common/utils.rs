use oxc_allocator::{Box, Vec};
use oxc_ast::ast::{Expression, FormalParameter, FunctionBody, IdentifierName};

pub fn get_fun_params<'a, 'b>(
    expression: &'b Expression<'a>,
) -> Option<&'b Vec<'a, FormalParameter<'a>>> {
    match expression {
        Expression::FunctionExpression(fun) => Some(&fun.params.items),
        Expression::ArrowFunctionExpression(fun) => Some(&fun.params.items),
        _ => None,
    }
}

pub fn get_fun_body<'a, 'b>(
    expression: &'b Expression<'a>,
) -> Option<&'b Box<'b, FunctionBody<'a>>> {
    match expression {
        Expression::FunctionExpression(fun) => fun.body.as_ref(),
        Expression::ArrowFunctionExpression(fun) => Some(&fun.body),
        _ => None,
    }
}

// TO-DO
pub fn is_iife(expression: &Expression<'_>) -> bool {
    matches!(
        expression.without_parentheses(),
        Expression::CallExpression(_)
    )
}

///  Return `true` if `require.r` exists.    
/// `require.r` is a webpack helper function
///  that defines `__esModule` on exports.
pub fn is_esm_helper(expr: &Expression<'_>) -> bool {
    let Expression::CallExpression(call_expr) = expr.without_parentheses() else {
        return false;
    };
    let Expression::StaticMemberExpression(mem) = &call_expr.callee.without_parentheses() else {
        return false;
    };
    let (Expression::Identifier(idf), IdentifierName { name, .. }) = (&mem.object, &mem.property)
    else {
        return false;
    };
    idf.name == "require" && name.as_str() == "r"
}
