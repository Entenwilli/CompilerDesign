use std::collections::HashMap;

use tracing::trace;

use crate::{
    lexer::token::{OperatorType, Token},
    parser::{ast::Tree, symbols::Name, types::Type},
    util::int_parsing::parse_int,
};

#[derive(Clone)]
pub struct AnalysisState {
    return_state: ReturnState,
    namespace: HashMap<Name, VariableStatus>,
    active_loops: usize,
}

impl Default for AnalysisState {
    fn default() -> AnalysisState {
        AnalysisState {
            return_state: ReturnState::NotReturing,
            namespace: HashMap::new(),
            active_loops: 0,
        }
    }
}

impl AnalysisState {
    pub fn enter_loop(&mut self) {
        self.active_loops += 1;
    }

    pub fn exit_loop(&mut self) {
        self.active_loops -= 1;
    }

    pub fn loop_active(&self) -> bool {
        self.active_loops > 0
    }
}

#[derive(PartialEq, Clone)]
enum ReturnState {
    Returning,
    NotReturing,
}

#[derive(Clone)]
pub struct VariableStatus {
    type_status: Type,
    declaration_status: DeclarationStatus,
}

impl VariableStatus {
    pub fn new(type_status: Type, declaration_status: DeclarationStatus) -> VariableStatus {
        VariableStatus {
            type_status,
            declaration_status,
        }
    }

    pub fn declaration(&self) -> &DeclarationStatus {
        &self.declaration_status
    }

    pub fn type_status(&self) -> &Type {
        &self.type_status
    }

    pub fn set_initialized(&mut self) {
        self.declaration_status = DeclarationStatus::Initialized
    }
}

#[derive(PartialEq, PartialOrd, Clone)]
pub enum DeclarationStatus {
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
            analyze(sub.clone(), state)?;
            if get_variable_type(sub, state)
                .ok_or("Variable not defined!")?
                .ne(&Type::Int)
            {
                return Err("Function must return an int".to_string());
            }
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
                                .declaration()
                                .ne(&DeclarationStatus::Initialized)
                            {
                                state.namespace.get_mut(&name).unwrap().set_initialized();
                            }
                            let expression_type = get_variable_type(expression.clone(), state)
                                .ok_or("Variable undefined!")?;
                            if state
                                .namespace
                                .get(&name)
                                .unwrap()
                                .type_status()
                                .ne(&expression_type)
                            {
                                return Err(
                                    "Variable must be of the same type as expression!".to_string()
                                );
                            }
                            {}
                        } else if !state.namespace.contains_key(&name) {
                            return Err(format!("Undecleared variable {} used!", name.as_string()));
                        } else if state
                            .namespace
                            .get(&name)
                            .unwrap()
                            .declaration()
                            .eq(&DeclarationStatus::Declared)
                        {
                            return Err("Decleared variable without value used!".to_string());
                        }
                    }
                }
            };
            Ok(())
        }
        Tree::Declaration(variable_type, name, initializer) => {
            analyze(variable_type.clone(), state)?;
            analyze(name.clone(), state)?;
            let variable_type =
                get_variable_type(variable_type, state).ok_or("Variable undefined!")?;
            let variable_state = if initializer.is_some() {
                VariableStatus::new(variable_type.clone(), DeclarationStatus::Initialized)
            } else {
                VariableStatus::new(variable_type.clone(), DeclarationStatus::Declared)
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
                if get_variable_type(present_initializer.clone(), state)
                    .ok_or("Variable undefined!")?
                    .ne(&variable_type)
                {
                    trace!(
                        "Initializer is {:?}",
                        get_variable_type(present_initializer.clone(), state)
                    );
                    trace!("Variable should be {:?}", variable_type);
                    return Err("initializer must be of same type as variable".to_string());
                }
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
                    .declaration()
                    .eq(&DeclarationStatus::Declared)
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
        Tree::Continue(_) => {
            if !state.loop_active() {
                return Err("Continue can only be used in a loop".to_string());
            }
            Ok(())
        }
        Tree::Break(_) => {
            if !state.loop_active() {
                return Err("Break can only be used in a loop".to_string());
            }
            Ok(())
        }
        Tree::BinaryOperation(lhs, rhs, operator_type) => {
            match operator_type {
                OperatorType::LogicalOr | OperatorType::LogicalAnd => {
                    if get_variable_type(lhs.clone(), state)
                        .ok_or("Variable undefined!")?
                        .ne(&Type::Bool)
                        || get_variable_type(rhs.clone(), state)
                            .ok_or("Variable undefined!")?
                            .ne(&Type::Bool)
                    {
                        return Err("Expression must be a boolean".to_string());
                    }
                }
                OperatorType::Minus
                | OperatorType::ShiftRight
                | OperatorType::ShiftLeft
                | OperatorType::BitwiseXor
                | OperatorType::BitwiseAnd
                | OperatorType::BitwiseOr
                | OperatorType::Plus
                | OperatorType::Mul
                | OperatorType::Mod
                | OperatorType::Div => {
                    if get_variable_type(lhs.clone(), state)
                        .ok_or("Variable undefined!")?
                        .ne(&Type::Int)
                        || get_variable_type(rhs.clone(), state)
                            .ok_or("Variable undefined!")?
                            .ne(&Type::Int)
                    {
                        return Err("Expression must be a integer".to_string());
                    }
                }
                OperatorType::Lower
                | OperatorType::LowerEquals
                | OperatorType::Equals
                | OperatorType::NotEquals
                | OperatorType::Higher
                | OperatorType::HigherEquals => {
                    let lhs_type =
                        get_variable_type(lhs.clone(), state).ok_or("Variable undefined!")?;
                    let rhs_type =
                        get_variable_type(rhs.clone(), state).ok_or("Variable undefined!")?;
                    if lhs_type.ne(&rhs_type) {
                        return Err("Comparison operators must have equal types".to_string());
                    }
                }
                OperatorType::Assign
                | OperatorType::AssignMul
                | OperatorType::AssignDiv
                | OperatorType::AssignMod
                | OperatorType::AssignMinus
                | OperatorType::AssignShiftLeft
                | OperatorType::AssignBitwiseOr
                | OperatorType::AssignBitwiseNot
                | OperatorType::AssignBitwiseAnd
                | OperatorType::AssignBitwiseXor
                | OperatorType::AssignShiftRight
                | OperatorType::AssignPlus => {}
                OperatorType::LogicalNot
                | OperatorType::BitwiseNot
                | OperatorType::TernaryColon
                | OperatorType::TernaryQuestionMark => return Err("Invalid operator".to_string()),
            }
            analyze(lhs, state)?;
            analyze(rhs, state)
        }
        Tree::Block(statements, _) => {
            for statement in statements {
                analyze(Box::new(statement.clone()), state)?;
            }
            Ok(())
        }
        Tree::LValueIdentifier(name) => analyze(name, state),
        Tree::UnaryOperation(expression, operator_type, _) => {
            match operator_type {
                OperatorType::LogicalNot => {
                    if get_variable_type(expression.clone(), state)
                        .ok_or("Variable undefined!")?
                        .ne(&Type::Bool)
                    {
                        return Err("Expression must be a boolean".to_string());
                    }
                }
                OperatorType::BitwiseNot | OperatorType::Minus => {
                    if get_variable_type(expression.clone(), state)
                        .ok_or("Variable undefined!")?
                        .ne(&Type::Int)
                    {
                        return Err("Expression must be a integer".to_string());
                    }
                }
                _ => {}
            }
            analyze(expression, state)
        }
        Tree::Type(_, _) => Ok(()),
        Tree::Program(statements) => {
            for statement in statements {
                analyze(Box::new(statement), state)?;
            }
            Ok(())
        }
        Tree::TernaryOperation(statement, true_statement, false_statement) => {
            analyze(statement.clone(), state)?;
            if get_variable_type(statement, state)
                .ok_or("Variable undefined!")?
                .ne(&Type::Bool)
            {
                return Err("Condition must be a boolean".to_string());
            }
            let true_type =
                get_variable_type(true_statement.clone(), state).ok_or("Variable undefined!")?;
            let false_type =
                get_variable_type(false_statement.clone(), state).ok_or("Variable undefined!")?;
            if true_type.ne(&false_type) {
                return Err("Ternary operator types not equal".to_string());
            }
            analyze(true_statement, state)?;
            analyze(false_statement, state)
        }
        Tree::If(condition, expression, else_expression, _) => {
            analyze(condition.clone(), state)?;
            if get_variable_type(condition, state)
                .ok_or("Variable undefined!")?
                .ne(&Type::Bool)
            {
                return Err("Condition must be a boolean".to_string());
            }
            state.return_state = ReturnState::NotReturing;
            analyze(expression, state)?;
            let if_return_state = state.return_state.clone();
            if let Some(other_expression) = else_expression {
                state.return_state = ReturnState::NotReturing;
                analyze(other_expression, state)?;
                if state.return_state.eq(&ReturnState::Returning)
                    && if_return_state.eq(&ReturnState::Returning)
                {
                    state.return_state = ReturnState::Returning;
                } else {
                    state.return_state = ReturnState::NotReturing;
                }
            } else {
                state.return_state = ReturnState::NotReturing;
            }
            Ok(())
        }
        Tree::While(condition, expression, _) => {
            analyze(condition.clone(), state)?;
            if get_variable_type(condition, state)
                .ok_or("Variable undefined!")?
                .ne(&Type::Bool)
            {
                return Err("Condition must be a boolean".to_string());
            }
            state.enter_loop();
            analyze(expression, state)?;
            state.return_state = ReturnState::NotReturing;
            state.exit_loop();
            Ok(())
        }
        Tree::For(initializer, condition, updater, expression, _) => {
            if let Some(initializer_expression) = initializer {
                analyze(initializer_expression, state)?;
            }
            analyze(condition.clone(), state)?;
            if get_variable_type(condition, state)
                .ok_or("Variable undefined!")?
                .ne(&Type::Bool)
            {
                return Err("Condition must be a boolean".to_string());
            }
            if let Some(updater_expression) = updater {
                let old_state = state.clone();
                analyze(updater_expression, state)?;
                *state = old_state;
            }
            state.enter_loop();
            analyze(expression, state)?;
            state.return_state = ReturnState::NotReturing;
            state.exit_loop();
            Ok(())
        }
    }
}

fn get_variable_type(type_tree: Box<Tree>, state: &mut AnalysisState) -> Option<Type> {
    match *type_tree {
        Tree::Type(variable_type, _) => Some(variable_type),
        Tree::Literal(_, _, _) => Some(Type::Int),
        Tree::BoolLiteral(_, _) => Some(Type::Bool),
        Tree::BinaryOperation(_, _, operator_type) | Tree::UnaryOperation(_, operator_type, _) => {
            match operator_type {
                OperatorType::Mul
                | OperatorType::Div
                | OperatorType::Plus
                | OperatorType::Mod
                | OperatorType::ShiftLeft
                | OperatorType::ShiftRight
                | OperatorType::Minus => Some(Type::Int),
                OperatorType::Lower
                | OperatorType::LowerEquals
                | OperatorType::Equals
                | OperatorType::NotEquals
                | OperatorType::HigherEquals
                | OperatorType::Higher => Some(Type::Bool),
                OperatorType::BitwiseOr
                | OperatorType::BitwiseNot
                | OperatorType::BitwiseAnd
                | OperatorType::BitwiseXor => Some(Type::Int),
                OperatorType::LogicalOr | OperatorType::LogicalNot | OperatorType::LogicalAnd => {
                    Some(Type::Bool)
                }
                _ => None,
            }
        }
        Tree::IdentifierExpression(name) => {
            if let Tree::Name(name, _) = *name {
                Some(state.namespace.get(&name)?.type_status().clone())
            } else {
                None
            }
        }
        Tree::TernaryOperation(_, true_expression, _) => get_variable_type(true_expression, state),
        _ => None,
    }
}
