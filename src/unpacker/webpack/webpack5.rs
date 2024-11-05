use indexmap::IndexMap;

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::{
    ast::{
        CallExpression, Expression, ExpressionStatement, ObjectPropertyKind, Program, Statement,
    },
    AstBuilder, AstKind,
};
use oxc_semantic::{NodeId, SemanticBuilder};
use oxc_span::Span;

use crate::unpacker::{common::fun_renamer::FunctionParamRenamer, Module};

pub fn get_modules_form_webpack5<'a>(
    allocator: &'a Allocator,
    program: &Program<'a>,
) -> Option<std::vec::Vec<Module<'a>>> {
    let ast = AstBuilder::new(allocator);

    let semantic = SemanticBuilder::new("").build(program).semantic;

    let nodes = semantic.nodes();
    let program_source_type = nodes
        .root()
        .map(|id| nodes.get_node(id).kind().as_program().unwrap().source_type);

    let program_directives = nodes.root().map(|id| {
        nodes
            .get_node(id)
            .kind()
            .as_program()
            .unwrap()
            .directives
            .clone_in(allocator)
    });

    let Some(root_id) = nodes.root() else {
        return None;
    };

    let mut webpackBootstrap = NodeId::DUMMY;

    // println!("{:#?}", nodes.get_node(root_id));

    let mut moduleMap = IndexMap::new();
    let mut modules = vec![];

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
                                // println!("====> module_id {:?}", module_id);

                                let expr = obj_prop.value.without_parentheses();

                                match expr {
                                    Expression::ArrowFunctionExpression(_)
                                    | Expression::FunctionExpression(_) => {}
                                    _ => {
                                        continue;
                                    }
                                }

                                moduleMap.insert(module_id, expr);
                            }
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    for (module_id, expr) in moduleMap.iter() {
        println!("====> module_id {:?} {:?}", module_id, expr);

        let fun_statement = ast.statement_expression(Span::default(), expr.clone_in(allocator));

        let directives = program_directives
            .clone_in(allocator)
            .unwrap_or_else(|| ast.vec());

        let mut program = ast.program(
            Span::default(),
            program_source_type.unwrap().clone_in(allocator),
            None,
            directives,
            ast.vec1(fun_statement),
        );

        let mut fun_renamer =
            FunctionParamRenamer::new(allocator, ["module", "exports", "require"]);
        fun_renamer.build(&mut program);

        modules.push(Module::new(module_id.to_string(), false, program));
    }

    Some(modules)
}
