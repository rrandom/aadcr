use indexmap::IndexMap;

use oxc_allocator::Allocator;
use oxc_ast::{
    ast::{
        CallExpression, Expression, ExpressionStatement, ObjectPropertyKind, Program, Statement,
    },
    AstKind,
};
use oxc_semantic::{NodeId, SemanticBuilder};

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

    let mut webpackBootstrap = NodeId::DUMMY;

    // println!("{:#?}", nodes.get_node(root_id));

    let mut modules = IndexMap::new();

    for node in nodes.iter() {
        match node.kind() {
            AstKind::ExpressionStatement(ExpressionStatement { expression, .. })
                if nodes.parent_id(node.id()) == Some(root_id) =>
            {
                let Expression::CallExpression(call_expr) = expression.without_parentheses() else {
                    continue;
                };
                match call_expr.callee.without_parentheses() {
                    Expression::FunctionExpression(fun) => {
                        let Some(body) = &fun.body else {
                            continue;
                        };
                        for st in body.statements.iter() {
                            let Statement::VariableDeclaration(vardecl) = st else {
                                continue;
                            };
                            // println!("====> vardecl {:?}", vardecl);
                            let de = &vardecl.declarations[0];

                            let Some(init) = &de.init else {
                                continue;
                            };
                            let Expression::ObjectExpression(ob) = init.without_parentheses()
                            else {
                                continue;
                            };
                            for prop in ob.properties.iter() {
                                let ObjectPropertyKind::ObjectProperty(obj_prop) = prop else {
                                    continue;
                                };
                                println!("====> obj_prop {:?}", obj_prop.key);
                            }
                        }
                    }
                    Expression::ArrowFunctionExpression(fun) => {
                        let body = &fun.body;
                        for st in body.statements.iter() {
                            let Statement::VariableDeclaration(vardecl) = st else {
                                continue;
                            };
                            // println!("====> vardecl {:?}", vardecl);
                            let de = &vardecl.declarations[0];

                            let Some(init) = &de.init else {
                                continue;
                            };
                            let Expression::ObjectExpression(ob) = init.without_parentheses()
                            else {
                                continue;
                            };
                            for prop in ob.properties.iter() {
                                let ObjectPropertyKind::ObjectProperty(obj_prop) = prop else {
                                    continue;
                                };
                                // println!("====> obj_prop {:?}", obj_prop.value);
                                let Some(Expression::StringLiteral(s)) =
                                    obj_prop.key.as_expression()
                                else {
                                    continue;
                                };
                                let module_id = &s.value;
                                println!("====> module_id {:?}", module_id);

                                modules.insert(module_id, obj_prop.value.without_parentheses());
                            }
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    for (module_id, expr) in modules {
        println!("====> module_id {:?} {:?}", module_id, expr);
    }

    None
}
