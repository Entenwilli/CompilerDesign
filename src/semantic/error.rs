use std::fmt::Display;

use crate::{
    lexer::operator::{BinaryOperator, UnaryOperator},
    parser::{
        ast::{block_tree::BlockTree, expression_tree::ExpressionTree, name_tree::NameTree, Tree},
        symbols::Name,
    },
    util::span::Span,
};

pub enum SemanticError {
    LiteralInvalidBase,
    LiteralInvalid,
    UninitializedVariable(NameTree),
    UndefinedVariable(NameTree),
    RedeclaredVariable(NameTree),
    IncompatibleTypesAssign(NameTree, ExpressionTree),
    IncompatibleTypeOperation(BinaryOperator, ExpressionTree),
    IncompatibleTypeComparison(ExpressionTree, ExpressionTree),
    IncompatibleTypeTernary(ExpressionTree, ExpressionTree),
    IncompatibleTypeUnary(UnaryOperator, ExpressionTree),
    ContinueOutsideLoop(Span),
    BreakOutsideLoop(Span),
    ConditionMustBeBoolean(ExpressionTree),
    FunctionNotReturning(BlockTree),
    ForAdvancementDefinesVariable,
    NoMainFunction,
    MainMustReturnInt,
    IncompatibleReturnType(ExpressionTree),
    FunctionAlreadyDefined(Name),
    UndefinedFunction(Name),
    FunctionParameterMismatch,
    InvalidMainFunction,
}

impl Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticError::LiteralInvalidBase => writeln!(f, "Invalid Literal Base"),
            SemanticError::LiteralInvalid => writeln!(f, "Invalid Literal"),
            SemanticError::UndefinedVariable(variable) => writeln!(
                f,
                "Undefined variable {} at {}",
                variable.name().as_string(),
                variable.span()
            ),
            SemanticError::UninitializedVariable(variable) => writeln!(
                f,
                "Uninitialized variable {} at {}",
                variable.name().as_string(),
                variable.span()
            ),
            SemanticError::RedeclaredVariable(variable) => writeln!(
                f,
                "Variable {} redeclared at {}",
                variable.name().as_string(),
                variable.span()
            ),
            SemanticError::IncompatibleTypesAssign(variable, expression) => writeln!(
                f,
                "Expression at {} cannot assign to variable {}",
                expression.span(),
                variable.name().as_string()
            ),
            SemanticError::IncompatibleTypeOperation(operator, tree) => writeln!(
                f,
                "Expression at {} cannot be used with operator {}",
                tree.span(),
                operator
            ),
            SemanticError::IncompatibleTypeComparison(left, right) => writeln!(
                f,
                "The arguments at {} and {} cannot be compared",
                left.span(),
                right.span()
            ),
            SemanticError::IncompatibleTypeTernary(true_expression, false_expression) => {
                writeln!(
                    f,
                    "Inconsistent types between ternary arguments at {} and {}",
                    true_expression.span(),
                    false_expression.span()
                )
            }
            SemanticError::IncompatibleTypeUnary(operator, tree) => writeln!(
                f,
                "Expression at {} cannot be used with operator {}",
                tree.span(),
                operator
            ),
            SemanticError::ContinueOutsideLoop(span) => {
                writeln!(f, "Continue at {} cannot exist outside a loop", span)
            }
            SemanticError::BreakOutsideLoop(span) => {
                writeln!(f, "Break at {} cannot exist outside a loop", span)
            }
            SemanticError::ConditionMustBeBoolean(expression) => writeln!(
                f,
                "The condition at {} must be a boolean",
                expression.span()
            ),
            SemanticError::FunctionNotReturning(block) => {
                writeln!(f, "Function at {} is not returning!", block.span())
            }
            SemanticError::ForAdvancementDefinesVariable => {
                writeln!(f, "For advancement block cannot define a variable")
            }
            SemanticError::MainMustReturnInt => {
                writeln!(f, "Main function must return int")
            }
            SemanticError::IncompatibleReturnType(tree) => {
                writeln!(f, "Expression at {} returns wrong type", tree.span())
            }
            SemanticError::FunctionAlreadyDefined(name) => {
                writeln!(f, "Function with name {} already defined", name.as_string())
            }
            SemanticError::UndefinedFunction(name) => {
                writeln!(f, "Undefined function {}", name.as_string())
            }
            SemanticError::FunctionParameterMismatch => {
                writeln!(f, "Mismatching function parameters!")
            }
            SemanticError::NoMainFunction => {
                writeln!(f, "No main function!")
            }
            SemanticError::InvalidMainFunction => {
                writeln!(f, "Invalid main function")
            }
        }
    }
}
