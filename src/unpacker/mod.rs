use oxc_ast::{
    ast::{ArrayExpressionElement, Expression},
    AstKind,
};
use oxc_semantic::{NodeId, Semantic};
pub struct Module {}

pub fn get_modules_form_webpack4(semantic: &Semantic) -> Option<Module> {
    let mut moduleFactory = None;
    let mut moduleFactoryID = NodeId::new(0);

    for node in semantic.nodes() {
      println!("node: {:#?}", node.kind());
        match node.kind() {
        AstKind::CallExpression(call)
        /*if matches!(call.callee, Expression::ParenthesizedExpression(_)) */ =>
        {
          println!("ok");
                let args = call.arguments[0].as_expression().unwrap();
                match args {
                    Expression::ArrayExpression(arr) => {
                        let eles = &arr.elements;
                        let all_fun = eles
                            .iter()
                            .all(|d| matches!(d, ArrayExpressionElement::FunctionExpression(_)));
                        println!("Found ? {}", all_fun);
                        if all_fun {
                            moduleFactory = Some(call);
                            moduleFactoryID = node.id();
                        }
                    }
                    _ => {}
                }
                // break;
            }
            _ => {}
        }
        if moduleFactory.is_none() {
            return None;
        }
    }
    println!("Id: {:#?}", moduleFactoryID);
    None
}
