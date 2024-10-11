use oxc_ast::{
    ast::{ArrayExpressionElement, Expression},
    AstKind,
};
use oxc_semantic::{NodeId, Semantic};
pub struct Module {}

pub fn get_modules_form_webpack4(semantic: &Semantic) -> Option<Module> {

    for node in semantic.nodes().iter() {
        // println!("NOde==\n {:?}", node.kind());
        match node.kind() {
          AstKind::CallExpression(call)
          /*if matches!(call.callee, Expression::ParenthesizedExpression(_)) */ =>
          {
            println!("found Call {:?}", node.id());
            if (call.arguments.len() == 1) {
              match call.arguments[0].as_expression().unwrap() {
                Expression::ArrayExpression(arr) => {
                  let eles = &arr.elements;
                  let all_fun = eles
                            .iter()
                            .all(|d| matches!(d, ArrayExpressionElement::FunctionExpression(_)));
                        println!("Found ? {}", all_fun);
                }
                _ => {}
              }
            }
          }
          _ => {}
      }
    }

    return None;
}
