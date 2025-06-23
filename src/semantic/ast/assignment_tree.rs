use tracing::trace;

use crate::{
    lexer::operator::AssignmentOperator,
    parser::{ast::assignment_tree::AssignmentTree, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState, DeclarationStatus},
};

impl SemanticAnalysis for AssignmentTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        self.lvalue().analyze(state)?;
        self.expression().analyze(state)?;

        let operator = self.operator();
        let name = self.lvalue().identifier();
        if operator.eq(&AssignmentOperator::Assign) {
            trace!(
                "Variable {:?} is now defined with type {:?}!",
                name,
                self.expression().r#type(state)?
            );
            state
                .namespace
                .get(&name.name())
                .ok_or(SemanticError::UndefinedVariable(name.clone()))?;
            state
                .namespace
                .get_mut(&name.name())
                .unwrap()
                .set_initialized();
            let expression_type = self.expression().r#type(state)?;
            if state
                .namespace
                .get(&name.name())
                .unwrap()
                .type_status()
                .ne(&expression_type)
            {
                return Err(SemanticError::IncompatibleTypesAssign(
                    name.clone(),
                    self.expression().clone(),
                ));
            }
        } else if !state.namespace.contains_key(&name.name()) {
            return Err(SemanticError::UndefinedVariable(name.clone()));
        } else if state
            .namespace
            .get(&name.name())
            .unwrap()
            .declaration()
            .eq(&DeclarationStatus::Declared)
            && state.is_reachable()
        {
            return Err(SemanticError::UninitializedVariable(name.clone()));
        }
        let lhs_type = self.lvalue().r#type(state)?;
        let rhs_type = self.expression().r#type(state)?;
        trace!("Assignment types: {:?} {:?}", lhs_type, rhs_type);
        match self.operator() {
            AssignmentOperator::Assign => {
                if lhs_type.ne(&rhs_type) {
                    return Err(SemanticError::IncompatibleTypesAssign(
                        name.clone(),
                        self.expression().clone(),
                    ));
                }
            }
            AssignmentOperator::AssignPlus
            | AssignmentOperator::AssignMinus
            | AssignmentOperator::AssignMul
            | AssignmentOperator::AssignDiv
            | AssignmentOperator::AssignMod
            | AssignmentOperator::AssignShiftLeft
            | AssignmentOperator::AssignShiftRight => {
                if lhs_type.ne(&Type::Int) || rhs_type.ne(&Type::Int) {
                    return Err(SemanticError::IncompatibleTypesAssign(
                        name.clone(),
                        self.expression().clone(),
                    ));
                }
            }
            AssignmentOperator::AssignBitwiseNot
            | AssignmentOperator::AssignBitwiseAnd
            | AssignmentOperator::AssignBitwiseOr
            | AssignmentOperator::AssignBitwiseXor => {
                if lhs_type.ne(&Type::Int) || rhs_type.ne(&Type::Int) {
                    return Err(SemanticError::IncompatibleTypesAssign(
                        name.clone(),
                        self.expression().clone(),
                    ));
                }
            }
        }
        Ok(())
    }

    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}
