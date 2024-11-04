use oxc_allocator::Allocator;
use oxc_ast::{
    ast::{CallExpression, Expression, ExpressionStatement, Program},
    AstKind,
};
use oxc_semantic::SemanticBuilder;

use crate::unpacker::Module;

pub fn get_modules_form_webpack5<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
) -> Option<std::vec::Vec<Module<'a>>> {
    let semantic = SemanticBuilder::new("").build(program).semantic;

    let nodes = semantic.nodes();

    let Some(root_id) = nodes.root() else {
        return None;
    };

    // println!("{:#?}", nodes.get_node(root_id));

    for node in nodes.iter() {
        match node.kind() {
            AstKind::ExpressionStatement(ExpressionStatement { expression, .. }) => {
                // println!("{:#?}", expression.without_parentheses());
                if let Expression::CallExpression(call_expr) = expression.without_parentheses() {
                    match call_expr.callee.without_parentheses() {
                        Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_)
                            if Some(root_id) == nodes.parent_id(node.id()) =>
                        {
                            println!("Find {:?}", call_expr.callee);
                        }
                        _ => {
                            // println!("{:?}", call_expr.callee);
                        }
                    }
                }
            }
            _ => {}
        }
    }

    None
}
