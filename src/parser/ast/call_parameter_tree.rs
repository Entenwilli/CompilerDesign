use std::fmt::Display;

use crate::{
    lexer::collection::ParserTokens,
    parser::{
        ast::{expression_tree::ExpressionTree, Tree},
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct CallParameterTree {
    expression: ExpressionTree,
}

impl Tree for CallParameterTree {
    fn span(&self) -> Span {
        self.expression.span()
    }

    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        let expression = ExpressionTree::from_tokens(tokens)?;
        Ok(CallParameterTree { expression })
    }
}

impl CallParameterTree {
    pub fn expression(&self) -> &ExpressionTree {
        &self.expression
    }
}

impl Display for CallParameterTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression)
    }
}
