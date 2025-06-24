use tracing::trace;

use crate::{
    parser::{ast::program_tree::ProgramTree, symbols::Name, types::Type},
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for ProgramTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        trace!("Running semantic analysis on program");
        state.register_function(
            Name::IdentifierName("print".to_string()),
            Type::Int,
            vec![Type::Int],
        )?;
        state.register_function(Name::IdentifierName("read".to_string()), Type::Int, vec![])?;
        state.register_function(Name::IdentifierName("flush".to_string()), Type::Int, vec![])?;
        for function in self.functions() {
            let name = function.name_tree().name().clone();
            let return_type = function.return_type().type_tree().clone();
            let parameter = function
                .parameters()
                .iter()
                .map(|v| v.type_tree().type_tree().clone())
                .collect();
            state.register_function(name, return_type, parameter)?;
        }
        for function in self.functions() {
            function.analyze(state)?;
        }
        Ok(())
    }
    fn r#type(&self, _: &mut AnalysisState) -> Result<Type, SemanticError> {
        Ok(Type::Unit)
    }
}
