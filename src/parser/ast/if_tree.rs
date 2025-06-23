use std::fmt::Display;

use crate::{
    lexer::{
        collection::ParserTokens,
        token::{KeywordType, SeperatorType},
    },
    parser::{
        ast::{expression_tree::ExpressionTree, statement_tree::StatementTree, Tree},
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct IfTree {
    condition: ExpressionTree,
    if_statement: Box<StatementTree>,
    else_statement: Option<Box<StatementTree>>,
}

impl Tree for IfTree {
    fn span(&self) -> Span {
        if let Some(else_statement) = &self.else_statement {
            self.condition.span().merge(&else_statement.span())
        } else {
            self.condition.span().merge(&self.if_statement.span())
        }
    }
    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        tokens.expect_keyword(KeywordType::If)?;
        tokens.expect_seperator(SeperatorType::ParenOpen)?;
        let condition = ExpressionTree::from_tokens(tokens)?;
        tokens.expect_seperator(SeperatorType::ParenClose)?;
        let if_statement = Box::new(StatementTree::from_tokens(tokens)?);
        if let Some(else_token) = tokens.peek() {
            if else_token.is_keyword(&KeywordType::Else) {
                tokens.consume()?;
                let else_statement = Box::new(StatementTree::from_tokens(tokens)?);
                return Ok(IfTree {
                    condition,
                    if_statement,
                    else_statement: Some(else_statement),
                });
            }
        }
        Ok(IfTree {
            condition,
            if_statement,
            else_statement: None,
        })
    }
}

impl IfTree {
    pub fn condition(&self) -> &ExpressionTree {
        &self.condition
    }
    pub fn if_statement(&self) -> &StatementTree {
        &self.if_statement
    }
    pub fn else_statement(&self) -> &Option<Box<StatementTree>> {
        &self.else_statement
    }
}

impl Display for IfTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if({}) {}", self.condition, self.if_statement)?;
        if let Some(else_statement) = &self.else_statement {
            write!(f, "else {}", else_statement)?;
        }
        Ok(())
    }
}
