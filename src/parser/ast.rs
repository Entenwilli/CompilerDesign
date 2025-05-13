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
    Name(Name, Span),
    Negate(Box<Tree>, Span),
    Return(Box<Tree>, Position),
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
            Tree::Negate(expression, span) => span.clone().merge(expression.span()),
            Tree::Return(expression, start) => {
                Span::new(start.clone(), expression.span().end_owned())
            }
            Tree::Type(_, span) => span.clone(),
            Tree::Program(trees) => {
                let first = trees.first().unwrap();
                let last = trees.last().unwrap();
                return Span::new(first.span().start().clone(), last.span().end().clone());
            }
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
                write!(f, "{} {} {}", lvalue, operator.as_string(), expression)
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
                    write!(
                        f,
                        "{}: {} <- {}",
                        name,
                        type_tree,
                        initializer.clone().unwrap()
                    )
                } else {
                    write!(f, "{}: {}", name, type_tree)
                }
            }
            Tree::IdentifierExpression(identifier) => {
                write!(f, "{}", identifier)
            }
            Tree::Literal(value, _, _) => {
                write!(f, "{}", value)
            }
            Tree::Negate(tree, _) => {
                write!(f, "!{}", tree)
            }
            Tree::Return(expresion, _) => {
                write!(f, "return {}", expresion)
            }
            Tree::Type(type_tree, _) => {
                write!(f, "{}", type_tree.as_string())
            }
        }
    }
}
