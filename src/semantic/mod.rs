use std::collections::HashMap;

use crate::parser::{ast::Tree, symbols::Name};

pub struct AnalysisState {
    return_state: ReturnState,
    namespace: HashMap<Name, VariableStatus>,
}

impl Default for AnalysisState {
    fn default() -> AnalysisState {
        AnalysisState {
            return_state: ReturnState::NotReturing,
            namespace: HashMap::new(),
        }
    }
}

#[derive(PartialEq)]
enum ReturnState {
    Returning,
    NotReturing,
}

#[derive(PartialEq, PartialOrd)]
enum VariableStatus {
    Declared,
    Initialized,
}

pub fn analyze(tree: Box<Tree>, state: &mut AnalysisState) {
    match *tree {
        Tree::Literal(value, base, _) => {
            if i32::from_str_radix(value.as_str(), base as u32).is_err() {
                panic!("Invalid integer literal!");
            }
        }
        Tree::Return(sub, _) => {
            analyze(sub, state);
            state.return_state = ReturnState::Returning;
        }
        Tree::Function(return_type, name, body) => {
            analyze(return_type, state);
            analyze(name, state);
            analyze(body, state);
            if state.return_state.eq(&ReturnState::NotReturing) {
                panic!("Function not returing!");
            }
            state.return_state = ReturnState::NotReturing;
        }
        Tree::Assignment(lvalue, _, expression) => {
            analyze(lvalue.clone(), state);
            analyze(expression.clone(), state);
            if let Tree::LValueIdentifier(identifier) = *lvalue {
                if let Tree::Name(name, _) = *identifier {
                    state.namespace.get(&name).expect("Undeclared variable!");
                }
            };
        }
        Tree::Declaration(variable_type, name, initializer) => {
            analyze(variable_type, state);
            analyze(name.clone(), state);
            let variable_state = if initializer.is_some() {
                VariableStatus::Initialized
            } else {
                VariableStatus::Declared
            };
            if let Tree::Name(identifier, _) = *name {
                if state
                    .namespace
                    .get(&identifier)
                    .unwrap()
                    .ge(&variable_state)
                {
                    panic!("Reinitializing or redeclaring variables are not allowed!");
                }
                state.namespace.insert(identifier.clone(), variable_state);
            }
            if let Some(present_initializer) = initializer {
                analyze(present_initializer, state);
            }
        }
        Tree::IdentifierExpression(identifier) => {
            analyze(identifier.clone(), state);
            if let Tree::Name(name, _) = *identifier {
                state.namespace.get(&name).expect("Undeclared variable!");
            }
        }
        Tree::Name(_, _) => (),
        Tree::BinaryOperation(lhs, rhs, _) => {
            analyze(lhs, state);
            analyze(rhs, state);
        }
        Tree::Block(statements, _) => {
            for statement in statements {
                analyze(Box::new(statement), state);
            }
        }
        Tree::LValueIdentifier(name) => {
            analyze(name, state);
        }
        Tree::Negate(expression, _) => {
            analyze(expression, state);
        }
        Tree::Type(_, _) => (),
        Tree::Program(statements) => {
            for statement in statements {
                analyze(Box::new(statement), state);
            }
        }
    }
}
