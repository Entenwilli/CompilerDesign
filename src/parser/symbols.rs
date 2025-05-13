use crate::lexer::token::KeywordType;

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
pub enum Name {
    KeywordName(KeywordType),
    IdentifierName(String),
}

impl Name {
    pub fn as_string(&self) -> &str {
        match self {
            Name::KeywordName(keyword) => keyword.as_str(),
            Name::IdentifierName(identifier) => identifier.as_str(),
        }
    }
}
