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
pub struct WhileTree {
    span: Span,
    condition: Box<ExpressionTree>,
    statement: Box<StatementTree>,
}

impl Tree for WhileTree {
    fn span(&self) -> Span {
        self.span.clone()
    }
    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        let keyword = tokens.expect_keyword(KeywordType::While)?;
        tokens.expect_seperator(SeperatorType::ParenOpen)?;
        let condition = Box::new(ExpressionTree::from_tokens(tokens)?);
        tokens.expect_seperator(SeperatorType::ParenClose)?;
        let statement = Box::new(StatementTree::from_tokens(tokens)?);
        let span = keyword.span().merge(&statement.span());
        Ok(WhileTree {
            span,
            condition,
            statement,
        })
    }
}

impl WhileTree {
    pub fn condition(&self) -> &ExpressionTree {
        &self.condition
    }
    pub fn statement(&self) -> &StatementTree {
        &self.statement
    }
}

impl Display for WhileTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "while({}) ", self.condition)?;
        write!(f, "{}", self.statement)
    }
}
