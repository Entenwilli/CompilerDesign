use std::collections::HashMap;

use crate::{
    parser::{symbols::Name, types::Type},
    semantic::error::SemanticError,
};

pub mod ast;
pub mod error;

#[derive(Clone, Debug)]
pub struct AnalysisState {
    return_state: ReturnState,
    namespace: HashMap<Name, VariableStatus>,
    functions: HashMap<Name, (Type, Vec<Type>)>,
    active_loops: usize,
    reachable: bool,
    return_type: Type,
}

impl Default for AnalysisState {
    fn default() -> AnalysisState {
        AnalysisState {
            return_state: ReturnState::NotReturing,
            namespace: HashMap::new(),
            functions: HashMap::new(),
            active_loops: 0,
            reachable: true,
            return_type: Type::Unit,
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

    pub fn is_reachable(&self) -> bool {
        self.reachable
    }

    pub fn set_reachable(&mut self) {
        self.reachable = true;
    }

    pub fn set_unreachable(&mut self) {
        self.reachable = false;
    }

    pub fn return_type(&self) -> &Type {
        &self.return_type
    }

    pub fn set_return_type(&mut self, return_type: Type) {
        self.return_type = return_type;
    }

    pub fn register_function(
        &mut self,
        name: Name,
        return_type: Type,
        parameter: Vec<Type>,
    ) -> Result<(), SemanticError> {
        if self.functions.contains_key(&name) {
            Err(SemanticError::FunctionAlreadyDefined(name))
        } else {
            self.functions.insert(name, (return_type, parameter));
            Ok(())
        }
    }

    pub fn get_function_return_type(&mut self, name: &Name) -> Result<&Type, SemanticError> {
        Ok(&self
            .functions
            .get(name)
            .ok_or(SemanticError::UndefinedFunction(name.clone()))?
            .0)
    }

    pub fn get_function_parameter_info(
        &mut self,
        name: &Name,
    ) -> Result<&Vec<Type>, SemanticError> {
        Ok(&self
            .functions
            .get(name)
            .ok_or(SemanticError::UndefinedFunction(name.clone()))?
            .1)
    }
}

#[derive(PartialEq, Clone, Debug)]
enum ReturnState {
    Returning,
    NotReturing,
}

#[derive(Clone, Debug)]
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

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum DeclarationStatus {
    Declared,
    Initialized,
}
