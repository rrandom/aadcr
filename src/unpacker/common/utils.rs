use oxc_allocator::{Box, Vec};
use oxc_ast::ast::{Expression, FormalParameter, FunctionBody};

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
