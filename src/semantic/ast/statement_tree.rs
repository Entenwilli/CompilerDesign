use crate::{
    parser::{
        ast::statement_tree::{ControlStatementTree, SimpleStatementTree, StatementTree},
        types::Type,
    },
    semantic::{ast::SemanticAnalysis, error::SemanticError, AnalysisState},
};

impl SemanticAnalysis for StatementTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        match self {
            StatementTree::SimpleStatement(tree) => tree.analyze(state),
            StatementTree::ControlStatement(tree) => tree.analyze(state),
            StatementTree::BlockStatement(tree) => tree.analyze(state),
        }
    }

    fn r#type(&self, state: &mut AnalysisState) -> Result<Type, SemanticError> {
        match self {
            StatementTree::SimpleStatement(tree) => tree.r#type(state),
            StatementTree::ControlStatement(tree) => tree.r#type(state),
            StatementTree::BlockStatement(tree) => tree.r#type(state),
        }
    }
}

impl SemanticAnalysis for SimpleStatementTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        match self {
            SimpleStatementTree::AssignmentTree(tree) => tree.analyze(state),
            SimpleStatementTree::DeclerationTree(tree) => tree.analyze(state),
            SimpleStatementTree::CallTree(tree) => tree.analyze(state),
        }
    }

    fn r#type(&self, state: &mut AnalysisState) -> Result<Type, SemanticError> {
        match self {
            SimpleStatementTree::AssignmentTree(tree) => tree.r#type(state),
            SimpleStatementTree::DeclerationTree(tree) => tree.r#type(state),
            SimpleStatementTree::CallTree(tree) => tree.r#type(state),
        }
    }
}

impl SemanticAnalysis for ControlStatementTree {
    fn analyze(&self, state: &mut AnalysisState) -> Result<(), SemanticError> {
        match self {
            ControlStatementTree::IfTree(tree) => tree.analyze(state),
            ControlStatementTree::ForTree(tree) => tree.analyze(state),
            ControlStatementTree::ReturnTree(tree) => tree.analyze(state),
            ControlStatementTree::WhileTree(tree) => tree.analyze(state),
            ControlStatementTree::BreakTree(tree) => tree.analyze(state),
            ControlStatementTree::ContinueTree(tree) => tree.analyze(state),
        }
    }
    fn r#type(&self, state: &mut AnalysisState) -> Result<Type, SemanticError> {
        match self {
            ControlStatementTree::IfTree(tree) => tree.r#type(state),
            ControlStatementTree::ForTree(tree) => tree.r#type(state),
            ControlStatementTree::ReturnTree(tree) => tree.r#type(state),
            ControlStatementTree::WhileTree(tree) => tree.r#type(state),
            ControlStatementTree::BreakTree(tree) => tree.r#type(state),
            ControlStatementTree::ContinueTree(tree) => tree.r#type(state),
        }
    }
}
