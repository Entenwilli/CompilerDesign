use crate::{lexer::collection::ParserTokens, parser::error::ParseError, util::span::Span};

pub mod assignment_tree;
pub mod binary_operation_tree;
pub mod block_tree;
pub mod break_tree;
pub mod call_tree;
pub mod continue_tree;
pub mod declaration_tree;
pub mod expression_tree;
pub mod for_tree;
pub mod function_parameter_tree;
pub mod function_tree;
pub mod identifier_tree;
pub mod if_tree;
pub mod literal_tree;
pub mod lvalue_tree;
pub mod name_tree;
pub mod program_tree;
pub mod return_tree;
pub mod statement_tree;
pub mod ternary_operation_tree;
pub mod type_tree;
pub mod unary_operation_tree;
pub mod while_tree;

pub trait Tree {
    fn span(&self) -> Span;
    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError>
    where
        Self: Sized;
}
