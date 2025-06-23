use crate::{
    parser::{ast::ternary_operation_tree::TernaryOperationTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for TernaryOperationTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        self.condition().analyze(state)?;
        if self.condition().r#type(state)?.ne(&Type::Bool) {
            return Err(SemanticError::ConditionMustBeBoolean(
                self.condition().clone(),
            ));
        }
        let true_type = self.true_expression().r#type(state)?;
        let false_type = self.false_expression().r#type(state)?;
        if true_type.ne(&false_type) {
            return Err(SemanticError::IncompatibleTypeTernary(
                self.true_expression().clone(),
                self.false_expression().clone(),
            ));
        }
        self.true_expression().analyze(state)?;
        self.false_expression().analyze(state)
    }
    fn r#type(&self, state: &mut AnalysisState) -> Result<Type, SemanticError> {
        self.true_expression().r#type(state)
    }
}
