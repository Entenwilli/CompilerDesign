use crate::{
    parser::{ast::while_tree::WhileTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for WhileTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        let reachability = state.is_reachable();
        let returning_state = state.return_state.clone();
        let old_namespace = state.namespace.clone();
        self.condition().analyze(state)?;
        if self.condition().r#type(state)?.ne(&Type::Bool) {
            return Err(SemanticError::ConditionMustBeBoolean(
                self.condition().clone(),
            ));
        }
        state.enter_loop();
        self.statement().analyze(state)?;
        state.return_state = returning_state;
        state.namespace = old_namespace;
        state.exit_loop();
        if reachability {
            state.set_reachable();
        }
        Ok(())
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}
