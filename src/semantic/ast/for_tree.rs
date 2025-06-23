use tracing::trace;

use crate::{
    parser::{ast::for_tree::ForTree, symbols::Name, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState, DeclarationStatus},
};

impl SemanticAnalysis for ForTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        trace!("Running semantic analysis on {:?}", self);
        let reachability = state.is_reachable();
        let mut old_namespace = state.namespace.clone();
        let returning_state = state.return_state.clone();
        if let Some(initializer_expression) = self.initializer() {
            initializer_expression.analyze(state)?;
            for initialized_variable in state
                .namespace
                .iter()
                .filter(|v| old_namespace.contains_key(v.0))
                .filter(|v| v.1.declaration().eq(&DeclarationStatus::Initialized))
                .map(|v| v.0)
                .collect::<Vec<&Name>>()
            {
                old_namespace
                    .get_mut(initialized_variable)
                    .unwrap()
                    .set_initialized();
            }
        }
        self.condition().analyze(state)?;
        if self.condition().r#type(state)?.ne(&Type::Bool) {
            return Err(SemanticError::ConditionMustBeBoolean(
                self.condition().clone(),
            ));
        }
        state.enter_loop();
        self.statement().analyze(state)?;
        state.return_state = returning_state;
        state.exit_loop();

        if let Some(updater_expression) = self.advancement() {
            trace!("Analysing advancement of for loop");
            let names = state.namespace.iter().map(|v| v.0).len();
            updater_expression.analyze(state)?;
            let additional_names = state.namespace.iter().map(|v| v.0).len();
            if names != additional_names {
                return Err(SemanticError::ForAdvancementDefinesVariable);
            }
        }
        state.namespace = old_namespace;
        if reachability {
            state.set_reachable();
        }
        Ok(())
    }

    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}
