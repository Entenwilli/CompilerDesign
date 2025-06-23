use tracing::trace;

use crate::{
    parser::{ast::if_tree::IfTree, types::Type},
    semantic::{
        ast::SemanticAnalysis, error::SemanticError, AnalysisState, DeclarationStatus, ReturnState,
    },
};

impl SemanticAnalysis for IfTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        let returning_state = state.return_state.clone();
        let old_namespace = state.namespace.clone();
        self.condition().analyze(state)?;
        if self.condition().r#type(state)?.ne(&Type::Bool) {
            return Err(SemanticError::ConditionMustBeBoolean(
                self.condition().clone(),
            ));
        }
        state.return_state = ReturnState::NotReturing;
        let old_reachability = state.is_reachable();
        self.if_statement().analyze(state)?;
        let true_namespace = state.namespace.clone();
        let true_reachability = state.is_reachable();
        state.set_reachable();
        state.namespace = old_namespace.clone();
        let if_return_state = state.return_state.clone();
        if let Some(other_expression) = self.else_statement() {
            state.return_state = ReturnState::NotReturing;
            other_expression.analyze(state)?;
            let else_return_state = state.return_state.clone();
            if state.return_state.eq(&ReturnState::Returning)
                && if_return_state.eq(&ReturnState::Returning)
            {
                state.return_state = ReturnState::Returning;
            } else {
                state.return_state = returning_state;
            }
            let false_namespace = state.namespace.clone();
            state.namespace = old_namespace;
            trace!("True: {:?}, False:{:?}", true_namespace, false_namespace);
            for (initialized_variable, _) in true_namespace
                .iter()
                .filter(|(_, v)| v.declaration().eq(&DeclarationStatus::Initialized))
            {
                if false_namespace.contains_key(initialized_variable)
                    && state.namespace.contains_key(initialized_variable)
                    && false_namespace
                        .get(initialized_variable)
                        .unwrap()
                        .declaration()
                        .eq(&DeclarationStatus::Initialized)
                {
                    state
                        .namespace
                        .get_mut(initialized_variable)
                        .unwrap()
                        .set_initialized();
                }
                if else_return_state.eq(&ReturnState::Returning) {
                    if let Some(variable) = state.namespace.get_mut(initialized_variable) {
                        variable.set_initialized();
                    }
                }
            }
            if if_return_state.eq(&ReturnState::Returning) {
                for (initialized_variable, _) in false_namespace {
                    state
                        .namespace
                        .get_mut(&initialized_variable)
                        .unwrap()
                        .set_initialized();
                }
            }
            if !old_reachability || (!true_reachability && !state.is_reachable()) {
                state.set_unreachable();
            } else {
                state.set_reachable();
            }
        } else {
            state.return_state = returning_state;
        }
        Ok(())
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}
