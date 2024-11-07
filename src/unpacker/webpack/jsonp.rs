use indexmap::IndexMap;
use std::cell::RefCell;

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::{
    ast::{
        Argument, ArrayExpressionElement, CallExpression, Expression, ExpressionStatement,
        IdentifierName, LogicalOperator, MemberExpression, ObjectPropertyKind, Program,
        PropertyKey, Statement,
    },
    AstBuilder, AstKind,
};
use oxc_semantic::{ScopeTree, SemanticBuilder, SymbolTable};
use oxc_span::Span;
use oxc_traverse::{Traverse, TraverseCtx};

use crate::unpacker::{
    common::{fun_to_program::FunctionToProgram, utils, ModuleExportsStore},
    Module,
};

pub fn get_modules_form_jsonp<'a>(
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

    // let mut module_map = IndexMap::new();
    // let mut modules = vec![];

    let self_variable_names = ["self", "window"];

    for node in nodes.iter() {
        if let AstKind::CallExpression(CallExpression {
            callee, arguments, ..
        }) = node.kind()
            && let Expression::StaticMemberExpression(callee) = callee.without_parentheses()
            && let [Argument::ArrayExpression(arg)] = arguments.as_slice()
            && let [ArrayExpressionElement::ArrayExpression(_), ArrayExpressionElement::ObjectExpression(arg2)] =
                &arg.elements.as_slice()[0..2]
        {
            if let Expression::AssignmentExpression(assign_expr) =
                callee.object.without_parentheses()
                && "push" == callee.property.name.as_str() && arg2.properties.len() > 0
            {
                let props = &arg2.properties;

                if let Some(MemberExpression::StaticMemberExpression(assign_left)) =
                    assign_expr.left.as_member_expression()
                    && let Expression::LogicalExpression(or_expr) = &assign_expr.right  && props.iter().all(|prop| {
                        let ObjectPropertyKind::ObjectProperty(prop) = prop else {
                            return false;
                        };

                        matches!(prop.key, PropertyKey::StringLiteral(_) |PropertyKey::NumericLiteral(_)) && matches!(prop.value.without_parentheses(), Expression::FunctionExpression(_))
                    })
                {
                    if let Expression::Identifier(idf) = assign_left.object.without_parentheses()
                        && or_expr.operator == LogicalOperator::Or
                        && let Expression::StaticMemberExpression(or_left) =
                            or_expr.left.without_parentheses()
                        && let Expression::ArrayExpression(or_right) =
                            or_expr.right.without_parentheses()
                    {
                        if self_variable_names.contains(&idf.name.as_str())
                            && let Expression::Identifier(or_left_obj) =
                                or_left.object.without_parentheses()
                            && self_variable_names.contains(&or_left_obj.name.as_str())
                            && or_right.elements.len() == 0
                        {
                            println!("{:?}", or_left_obj);
                        }
                    }
                }
            }
        }
    }

    None
}
