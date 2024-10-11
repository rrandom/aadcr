use oxc_ast::{
    ast::{
        ArrayExpressionElement, AssignmentExpression, Expression, MemberExpression, NumericLiteral,
    },
    AstKind,
};
use oxc_semantic::{NodeId, Semantic};
pub struct Module {}

pub fn get_modules_form_webpack4(semantic: &Semantic) -> Option<Module> {
    let mut entry_id = NodeId::DUMMY;
    for node in semantic.nodes().iter() {
        match node.kind() {
            AstKind::CallExpression(call) => {
                // println!("found Call {:?}", node.id());
                if call.arguments.len() == 1 {
                    if let Expression::ArrayExpression(arr) =
                        call.arguments[0].as_expression().unwrap()
                    {
                        let all_is_fun = arr
                            .elements
                            .iter()
                            .all(|d| matches!(d, ArrayExpressionElement::FunctionExpression(_)));
                        if all_is_fun {
                            println!("Found? {:?}", node.id());
                            entry_id = node.id();
                            // let span =  node.kind().as_call_expression().unwrap().span;
                        }
                    }
                }
            }
            AstKind::AssignmentExpression(AssignmentExpression {
                left,
                right: Expression::NumericLiteral(s),
                span,
                ..
            }) => {
                let contains = semantic
                    .nodes()
                    .ancestors(node.id())
                    .any(|id| id == entry_id);
                if !contains {
                  continue;
                }
                if let Some(MemberExpression::StaticMemberExpression(mm)) =
                    left.as_member_expression()
                {
                    if mm.object.is_identifier_reference() && mm.property.name.as_str() == "s" {
                        // println!("Member: {:?}", mm);
                        println!(
                            "assign: {} with {}",
                            span.source_text(semantic.source_text()),
                            s.value
                        );
                    }
                }
            }
            _ => {}
        }
    }

    None
}
