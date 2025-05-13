use crate::util::span::Span;

#[derive(Eq, Hash, PartialEq, Clone, Debug)]
pub enum KeywordType {
    Struct,
    If,
    Else,
    While,
    For,
    Continue,
    Break,
    Return,
    Assert,
    True,
    False,
    Null,
    Print,
    Read,
    Alloc,
    AllocArray,
    Int,
    Bool,
    Void,
    Char,
    String,
}

impl KeywordType {
    pub fn as_str(&self) -> &str {
        match self {
            Self::Struct => "struct",
            Self::If => "if",
            Self::Else => "else",
            Self::While => "while",
            Self::For => "for",
            Self::Continue => "continue",
            Self::Break => "break",
            Self::Return => "return",
            Self::Assert => "assert",
            Self::True => "true",
            Self::False => "false",
            Self::Null => "null",
            Self::Print => "print",
            Self::Read => "read",
            Self::Alloc => "alloc",
            Self::AllocArray => "alloc_array",
            Self::Int => "int",
            Self::Bool => "bool",
            Self::Void => "void",
            Self::Char => "char",
            Self::String => "string",
        }
    }

    pub fn from_string(keyword: &str) -> Option<Self> {
        match keyword {
            "struct" => Some(Self::Struct),
            "if" => Some(Self::If),
            "else" => Some(Self::Else),
            "while" => Some(Self::While),
            "for" => Some(Self::For),
            "continue" => Some(Self::Continue),
            "break" => Some(Self::Break),
            "return" => Some(Self::Return),
            "assert" => Some(Self::Assert),
            "true" => Some(Self::True),
            "false" => Some(Self::False),
            "null" => Some(Self::Null),
            "print" => Some(Self::Print),
            "read" => Some(Self::Read),
            "alloc" => Some(Self::Alloc),
            "alloc_array" => Some(Self::AllocArray),
            "int" => Some(Self::Int),
            "bool" => Some(Self::Bool),
            "void" => Some(Self::Void),
            "char" => Some(Self::Char),
            "string" => Some(Self::String),
            _ => None,
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum OperatorType {
    AssignMinus,
    Minus,
    AssignPlus,
    Plus,
    AssignMul,
    Mul,
    AssignDiv,
    Div,
    AssignMod,
    Mod,
    Assign,
}

impl OperatorType {
    pub fn is_assignment_operator(&self) -> bool {
        matches!(
            self,
            Self::Assign
                | Self::AssignMul
                | Self::AssignDiv
                | Self::AssignMod
                | Self::AssignPlus
                | Self::AssignMinus
        )
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::AssignMinus => "-=",
            Self::Minus => "-",
            Self::AssignPlus => "+=",
            Self::Plus => "+",
            Self::AssignMul => "*=",
            Self::Mul => "*",
            Self::AssignDiv => "/=",
            Self::Div => "/",
            Self::AssignMod => "%=",
            Self::Mod => "%",
            Self::Assign => "=",
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum SeperatorType {
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    Semicolon,
}

impl SeperatorType {
    fn as_str(&self) -> &str {
        match self {
            Self::ParenOpen => "(",
            Self::ParenClose => ")",
            Self::BraceOpen => "{",
            Self::BraceClose => "}",
            Self::Semicolon => ";",
        }
    }
}

#[derive(Clone, Debug)]
pub enum Token {
    ErrorToken(Span, String),
    Identifier(Span, String),
    Keyword(Span, KeywordType),
    NumberLiteral(Span, String, u64),
    Operator(Span, OperatorType),
    Separator(Span, SeperatorType),
}

impl Token {
    pub fn span(self) -> Span {
        match self {
            Self::ErrorToken(span, _) => span,
            Self::Identifier(span, _) => span,
            Self::Keyword(span, _) => span,
            Self::NumberLiteral(span, _, _) => span,
            Self::Operator(span, _) => span,
            Self::Separator(span, _) => span,
        }
    }
    pub fn is_keyword(&self, other_keyword_type: &KeywordType) -> bool {
        match self {
            Self::Keyword(_, keyword_type) => keyword_type.eq(other_keyword_type),
            _ => false,
        }
    }

    pub fn is_operator(&self, other_operator: &OperatorType) -> bool {
        match self {
            Self::Operator(_, operator) => operator.eq(other_operator),
            _ => false,
        }
    }

    pub fn is_assignment_operator(&self) -> bool {
        matches!(self, Self::Operator(_, operator) if operator.is_assignment_operator())
    }

    pub fn is_separator(&self, other_seperator: &SeperatorType) -> bool {
        match self {
            Self::Separator(_, seperator) => seperator.eq(other_seperator),
            _ => false,
        }
    }

    pub fn as_string(&self) -> &str {
        match self {
            Self::ErrorToken(_, value) => value.as_str(),
            Self::Identifier(_, value) => value.as_str(),
            Self::Keyword(_, keyword) => keyword.as_str(),
            Self::NumberLiteral(_, value, _) => value.as_str(),
            Self::Operator(_, operator) => operator.as_str(),
            Self::Separator(_, seperator) => seperator.as_str(),
        }
    }
}
