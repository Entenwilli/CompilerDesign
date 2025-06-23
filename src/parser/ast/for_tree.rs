use std::fmt::Display;

use tracing::trace;

use crate::{
    lexer::{
        collection::ParserTokens,
        token::{KeywordType, SeperatorType},
    },
    parser::{
        ast::{
            expression_tree::ExpressionTree,
            statement_tree::{SimpleStatementTree, StatementTree},
            Tree,
        },
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Debug, Clone)]
pub struct ForTree {
    initializer: Option<SimpleStatementTree>,
    condition: ExpressionTree,
    advancement: Option<SimpleStatementTree>,
    statement: Box<StatementTree>,
    span: Span,
}

impl Tree for ForTree {
    fn span(&self) -> Span {
        self.span.merge(&self.statement.span())
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        let keyword = tokens.expect_keyword(KeywordType::For)?;
        tokens.expect_seperator(SeperatorType::ParenOpen)?;
        let initializer = SimpleStatementTree::from_tokens(tokens).ok();
        trace!("Parsed initializer {:?}", initializer);
        tokens.expect_seperator(SeperatorType::Semicolon)?;
        let condition = ExpressionTree::from_tokens(tokens)?;
        tokens.expect_seperator(SeperatorType::Semicolon)?;
        trace!("Parsing advancement for for: {:?}", tokens.peek());
        let advancement = SimpleStatementTree::from_tokens(tokens).ok();
        trace!("Parsed advancement: {:?}", advancement);
        tokens.expect_seperator(SeperatorType::ParenClose)?;
        let statement = StatementTree::from_tokens(tokens)?;
        let span = keyword.span().merge(&statement.span());
        Ok(ForTree {
            initializer,
            condition,
            advancement,
            statement: Box::new(statement),
            span,
        })
    }
}

impl ForTree {
    pub fn initializer(&self) -> &Option<SimpleStatementTree> {
        &self.initializer
    }
    pub fn condition(&self) -> &ExpressionTree {
        &self.condition
    }
    pub fn advancement(&self) -> &Option<SimpleStatementTree> {
        &self.advancement
    }
    pub fn statement(&self) -> &StatementTree {
        &self.statement
    }
}

impl Display for ForTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "for(",)?;
        if let Some(initializer) = &self.initializer {
            write!(f, "{}", initializer)?;
        }
        write!(f, "; {}; ", self.condition)?;
        if let Some(advancement) = &self.advancement {
            write!(f, "{}", advancement)?;
        }
        write!(f, ") {}", self.statement)
    }
}
