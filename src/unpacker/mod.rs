use oxc_ast::{
    ast::{ArrayExpressionElement, Expression},
    AstKind,
};
use oxc_semantic::Semantic;
pub struct Module {}

pub fn get_modules_form_webpack4(semantic: &Semantic) -> Option<Module> {
    for node in semantic.nodes().iter() {
        if let AstKind::CallExpression(call) = node.kind() {
            // println!("found Call {:?}", node.id());
            if call.arguments.len() == 1 {
                if let Expression::ArrayExpression(arr) = call.arguments[0].as_expression().unwrap() {
                    let all_is_fun = arr.elements.iter().all(|d| {
                        matches!(d, ArrayExpressionElement::FunctionExpression(_))
                    });
                    if all_is_fun {
                        println!("Found? {:?}", node.id());
                        // let span =  node.kind().as_call_expression().unwrap().span;
                    }
                }
            }
        }
    }

    None
}
