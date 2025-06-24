use std::fmt::{Display, Error, Formatter};

use crate::{
    lexer::{collection::ParserTokens, token::SeperatorType},
    parser::{
        ast::{
            call_parameter_tree::CallParameterTree, identifier_tree::IdentifierExpressionTree, Tree,
        },
        error::ParseError,
    },
    util::span::Span,
};

#[derive(Clone, Debug)]
pub struct CallTree {
    identifier: IdentifierExpressionTree,
    call_parameter: Vec<CallParameterTree>,
    closing_span: Span,
}

impl Tree for CallTree {
    fn span(&self) -> Span {
        self.identifier.span().merge(&self.closing_span)
    }
    fn from_tokens(tokens: &mut ParserTokens) -> Result<Self, ParseError> {
        let identifier = IdentifierExpressionTree::from_tokens(tokens)?;
        tokens.expect_seperator(SeperatorType::ParenOpen)?;
        let mut call_parameter = Vec::new();
        if !tokens
            .peek()
            .ok_or(ParseError::ReachedEnd)?
            .is_separator(&SeperatorType::ParenClose)
        {
            call_parameter.push(CallParameterTree::from_tokens(tokens)?);
            while tokens
                .peek()
                .ok_or(ParseError::ReachedEnd)?
                .is_separator(&SeperatorType::Comma)
            {
                tokens.consume()?;
                call_parameter.push(CallParameterTree::from_tokens(tokens)?);
            }
        }
        let closing_span = tokens.expect_seperator(SeperatorType::ParenClose)?.span();
        Ok(CallTree {
            identifier,
            call_parameter,
            closing_span,
        })
    }
}

impl CallTree {
    pub fn identifier(&self) -> &IdentifierExpressionTree {
        &self.identifier
    }
    pub fn parameter(&self) -> &Vec<CallParameterTree> {
        &self.call_parameter
    }
}

impl Display for CallTree {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.identifier)?;
        write!(f, "(")?;
        for call_parameter in &self.call_parameter {
            write!(f, "{} ", call_parameter)?;
        }
        write!(f, ")")
    }
}
