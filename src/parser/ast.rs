use std::fmt::Display;

use crate::{
    lexer::token::{OperatorType, Token},
    parser::symbols::Name,
    parser::types::Type,
    util::{position::Position, span::Span},
};

#[derive(Clone, Debug)]
pub enum Tree {
    /// lvalue, operator, expression
    Assignment(Box<Tree>, Token, Box<Tree>),
    BinaryOperation(Box<Tree>, Box<Tree>, OperatorType),
    Block(Vec<Tree>, Span),
    Declaration(Box<Tree>, Box<Tree>, Option<Box<Tree>>),
    Function(Box<Tree>, Box<Tree>, Box<Tree>),
    IdentifierExpression(Box<Tree>),
    LValueIdentifier(Box<Tree>),
    Literal(String, u64, Span),
    BoolLiteral(bool, Span),
    TernaryOperation(Box<Tree>, Box<Tree>, Box<Tree>),
    Name(Name, Span),
    UnaryOperation(Box<Tree>, OperatorType, Span),
    Return(Box<Tree>, Position),
    Break(Span),
    Continue(Span),
    For(
        Option<Box<Tree>>,
        Box<Tree>,
        Option<Box<Tree>>,
        Box<Tree>,
        Span,
    ),
    While(Box<Tree>, Box<Tree>, Span),
    If(Box<Tree>, Box<Tree>, Option<Box<Tree>>, Span),
    Type(Type, Span),
    Program(Vec<Tree>),
}

impl Tree {
    pub fn span(&self) -> Span {
        match self {
            Tree::Assignment(lvalue, _, expression) => lvalue.span().merge(expression.span()),
            Tree::BinaryOperation(lhs, rhs, _) => lhs.span().merge(rhs.span()),
            Tree::Block(_, span) => span.clone(),
            Tree::Declaration(type_tree, name, initializer_option) => {
                if let Some(initializer) = initializer_option {
                    return type_tree.span().merge(initializer.span());
                }
                type_tree.span().merge(name.span())
            }
            Tree::Function(return_type, _name, body) => {
                Span::new(return_type.span().start_owned(), body.span().end_owned())
            }
            Tree::IdentifierExpression(name) => name.span(),
            Tree::LValueIdentifier(name) => name.span(),
            Tree::Literal(_, _, span) => span.clone(),
            Tree::Name(_, span) => span.clone(),
            Tree::UnaryOperation(expression, _, span) => span.clone().merge(expression.span()),
            Tree::Return(expression, start) => {
                Span::new(start.clone(), expression.span().end_owned())
            }
            Tree::Type(_, span) => span.clone(),
            Tree::Program(trees) => {
                let first = trees.first().unwrap();
                let last = trees.last().unwrap();
                return Span::new(first.span().start().clone(), last.span().end().clone());
            }
            Tree::BoolLiteral(_, span) => span.clone(),
            Tree::TernaryOperation(start, _, end) => start.span().merge(end.span()),
            Tree::Break(span) => span.clone(),
            Tree::Continue(span) => span.clone(),
            Tree::For(_, _, _, _, span) => span.clone(),
            Tree::While(_, _, span) => span.clone(),
            Tree::If(_, _, _, span) => span.clone(),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Tree::Name(name, _) => name.as_string(),
            _ => todo!(),
        }
    }
}

impl Display for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tree::Program(functions) => {
                write!(f, "[")?;
                for function in functions {
                    write!(f, "\n{}\n", function)?;
                }
                write!(f, "]")
            }
            Tree::Function(return_type, name, body) => {
                write!(
                    f,
                    "Function {} returning type {}:\n{}",
                    name, return_type, body
                )
            }
            Tree::Name(name, _) => {
                write!(f, "{}", name.as_string())
            }
            Tree::Assignment(lvalue, operator, expression) => {
                writeln!(f, "{} {} {}", lvalue, operator.as_string(), expression)
            }
            Tree::LValueIdentifier(identifier) => write!(f, "{}", identifier),
            Tree::BinaryOperation(lhs, rhs, operator) => {
                write!(f, "{} {} {}", lhs, operator.as_str(), rhs)
            }
            Tree::Block(operations, _) => {
                for operation in operations {
                    write!(f, "{}", operation)?
                }
                Ok(())
            }
            Tree::Declaration(type_tree, name, initializer) => {
                if initializer.is_some() {
                    writeln!(
                        f,
                        "{}: {} <- {}",
                        name,
                        type_tree,
                        initializer.clone().unwrap()
                    )
                } else {
                    writeln!(f, "{}: {}", name, type_tree)
                }
            }
            Tree::IdentifierExpression(identifier) => {
                write!(f, "{}", identifier)
            }
            Tree::Literal(value, _, _) => {
                write!(f, "{}", value)
            }
            Tree::UnaryOperation(tree, operator, _) => {
                write!(f, "{}{}", operator.as_str(), tree)
            }
            Tree::Return(expresion, _) => {
                writeln!(f, "return {}", expresion)
            }
            Tree::Type(type_tree, _) => {
                write!(f, "{}", type_tree.as_string())
            }
            Tree::BoolLiteral(value, _) => {
                write!(f, "{}", value.to_string())
            }
            Tree::TernaryOperation(expression, true_expression, false_expression) => {
                write!(
                    f,
                    "{} ? {} : {}",
                    expression, true_expression, false_expression
                )
            }
            Tree::Continue(_) => {
                writeln!(f, "continue")
            }
            Tree::Break(_) => {
                writeln!(f, "break")
            }
            Tree::For(init, condition, post, statement, _) => {
                writeln!(f, "for {:?}; {}; {:?} {{", init, condition, post)?;
                writeln!(f, "{}", statement)?;
                writeln!(f, "}}")
            }
            Tree::While(condition, statement, _) => {
                writeln!(f, "while {} {{", condition)?;
                writeln!(f, "{}", statement)?;
                writeln!(f, "}}")
            }
            Tree::If(condition, statement, else_statement, _) => {
                writeln!(f, "if {} {{", condition)?;
                writeln!(f, "{}", statement)?;
                if let Some(other_statement) = else_statement {
                    writeln!(f, "}} else {{")?;
                    writeln!(f, "{}", other_statement)?;
                    writeln!(f, "}}")
                } else {
                    writeln!(f, "}}")
                }
            }
        }
    }
}
