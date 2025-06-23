use tracing::trace;

use crate::{
    parser::{ast::declaration_tree::DeclarationTree, types::Type},
    semantic::{
        ast::SemanticAnalysis, error::SemanticError, AnalysisState, DeclarationStatus,
        VariableStatus,
    },
};

impl SemanticAnalysis for DeclarationTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        self.type_tree().analyze(state)?;
        self.name_tree().analyze(state)?;
        let variable_type = self.type_tree().r#type(state)?;
        let variable_state = if self.initializer_tree().is_some() {
            VariableStatus::new(variable_type.clone(), DeclarationStatus::Initialized)
        } else {
            VariableStatus::new(variable_type.clone(), DeclarationStatus::Declared)
        };
        if let Some(present_initializer) = self.initializer_tree() {
            if present_initializer.r#type(state)?.ne(&variable_type) {
                trace!("Initializer is {:?}", present_initializer.r#type(state)?);
                trace!("Variable should be {:?}", variable_type);
                return Err(SemanticError::IncompatibleTypesAssign(
                    self.name_tree().clone(),
                    present_initializer.clone(),
                ));
            }
            present_initializer.analyze(state)?;
        };
        let variable_status = state.namespace.get(&self.name_tree().name());
        if variable_status.is_some() {
            return Err(SemanticError::RedeclaredVariable(self.name_tree().clone()));
        }
        state
            .namespace
            .insert(self.name_tree().name().clone(), variable_state);
        trace!(
            "Variable {:?} is now declared as type {:?}",
            self.name_tree().name().as_string(),
            variable_type
        );
        Ok(())
    }

    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}
