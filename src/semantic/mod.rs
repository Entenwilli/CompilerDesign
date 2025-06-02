use std::collections::HashMap;

use crate::{
    lexer::token::{OperatorType, Token},
    parser::{ast::Tree, symbols::Name},
    util::int_parsing::parse_int,
};

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

#[must_use]
pub fn analyze(tree: Box<Tree>, state: &mut AnalysisState) -> Result<(), String> {
    match *tree {
        Tree::Literal(value, base, _) => {
            if base != 16 && base != 10 {
                return Err("Invalid base!".to_string());
            }
            parse_int(value, base).ok_or("Not valid integer literal!".to_string())?;
            Ok(())
        }
        Tree::Return(sub, _) => {
            analyze(sub, state)?;
            state.return_state = ReturnState::Returning;
            Ok(())
        }
        Tree::Function(return_type, name, body) => {
            analyze(return_type, state)?;
            analyze(name, state)?;
            analyze(body, state)?;
            if state.return_state.eq(&ReturnState::NotReturing) {
                return Err("Function not returing!".to_string());
            }
            state.return_state = ReturnState::NotReturing;
            Ok(())
        }
        Tree::Assignment(lvalue, operator, expression) => {
            analyze(lvalue.clone(), state)?;
            analyze(expression.clone(), state)?;
            if let Tree::LValueIdentifier(identifier) = *lvalue {
                if let Tree::Name(name, _) = *identifier {
                    if let Token::Operator(_, operator_type) = operator {
                        if let OperatorType::Assign = operator_type {
                            state
                                .namespace
                                .get(&name)
                                .ok_or(format!("Undeclared variable {}!", name.as_string()))?;
                            if state
                                .namespace
                                .get(&name)
                                .unwrap()
                                .ne(&VariableStatus::Initialized)
                            {
                                state.namespace.insert(name, VariableStatus::Initialized);
                            }
                        } else if !state.namespace.contains_key(&name) {
                            return Err(format!("Undecleared variable {} used!", name.as_string()));
                        } else if state
                            .namespace
                            .get(&name)
                            .unwrap()
                            .eq(&VariableStatus::Declared)
                        {
                            return Err("Decleared variable without value used!".to_string());
                        }
                    }
                }
            };
            Ok(())
        }
        Tree::Declaration(variable_type, name, initializer) => {
            analyze(variable_type, state)?;
            analyze(name.clone(), state)?;
            let variable_state = if initializer.is_some() {
                VariableStatus::Initialized
            } else {
                VariableStatus::Declared
            };
            if let Tree::Name(identifier, _) = *name {
                let variable_status = state.namespace.get(&identifier);
                if variable_status.is_some() {
                    return Err(
                        "Reinitializing or redeclaring variables are not allowed!".to_string()
                    );
                }
                state.namespace.insert(identifier.clone(), variable_state);
            }
            if let Some(present_initializer) = initializer {
                analyze(present_initializer, state)?;
            };
            Ok(())
        }
        Tree::IdentifierExpression(identifier) => {
            analyze(identifier.clone(), state)?;
            if let Tree::Name(name, _) = *identifier {
                state.namespace.get(&name).ok_or(format!(
                    "Undeclared variable {} used in expression!",
                    name.as_string()
                ))?;
                if state
                    .namespace
                    .get(&name)
                    .unwrap()
                    .eq(&VariableStatus::Declared)
                {
                    return Err(format!(
                        "Uninitialized variable {} used in expression!",
                        name.as_string()
                    ));
                }
            };
            Ok(())
        }
        Tree::Name(_, _) => Ok(()),
        Tree::BoolLiteral(_, _) => Ok(()),
        Tree::Continue(_) => Ok(()),
        Tree::Break(_) => Ok(()),
        Tree::BinaryOperation(lhs, rhs, _) => {
            analyze(lhs, state)?;
            analyze(rhs, state)
        }
        Tree::Block(statements, _) => {
            for statement in statements {
                analyze(Box::new(statement), state)?;
            }
            Ok(())
        }
        Tree::LValueIdentifier(name) => analyze(name, state),
        Tree::UnaryOperation(expression, _, _) => analyze(expression, state),
        Tree::Type(_, _) => Ok(()),
        Tree::Program(statements) => {
            for statement in statements {
                analyze(Box::new(statement), state)?;
            }
            Ok(())
        }
        Tree::TernaryOperation(statement, true_statement, false_statement) => {
            analyze(statement, state)?;
            analyze(true_statement, state)?;
            analyze(false_statement, state)
        }
        Tree::If(condition, expression, else_expression, _) => {
            analyze(condition, state)?;
            analyze(expression, state)?;
            if let Some(other_expression) = else_expression {
                analyze(other_expression, state)?;
            }
            Ok(())
        }
        Tree::While(condition, expression, _) => {
            analyze(condition, state)?;
            analyze(expression, state)
        }
        Tree::For(initializer, condition, updater, expression, _) => {
            if let Some(initializer_expression) = initializer {
                analyze(initializer_expression, state)?;
            }
            analyze(condition, state)?;
            if let Some(updater_expression) = updater {
                analyze(updater_expression, state)?;
            }
            analyze(expression, state)
        }
    }
}
