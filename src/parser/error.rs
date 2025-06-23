use std::fmt::Display;

use crate::lexer::token::{KeywordType, OperatorType, SeperatorType};

#[derive(PartialEq, Debug)]
pub enum ParseError {
    ExpectedKeyword(KeywordType),
    ExpectedSeparator(SeperatorType),
    ExpectedOperator(OperatorType),
    ExpectedIdentifier,
    ExpectedType,
    ExpectedAssignmentOperator,
    ExpectedUnaryOperator,
    ExpectedBinaryOperator,
    ExpectedTernaryOperator,
    ExpectedLiteral,
    ExpectedKeywordOrIdentifier,
    NotAnExpression,
    NotAStatement,
    NotAOperation,
    NoFunctions,
    NoMainFunction,
    ReachedEnd,
    WhitespaceError,
    InvalidCharacter,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: Proper implementation
        write!(f, "{:?}", self)
    }
}
