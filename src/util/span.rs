use std::fmt::Display;

use crate::util::position::Position;

#[derive(Clone, Debug)]
pub struct Span {
    start: Position,
    end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Span {
        Span { start, end }
    }

    pub fn start(&self) -> &Position {
        &self.start
    }

    pub fn start_owned(self) -> Position {
        self.end
    }

    pub fn end(&self) -> &Position {
        &self.end
    }

    pub fn end_owned(self) -> Position {
        self.end
    }

    pub fn merge(self, later: Span) -> Span {
        Span::new(self.start, later.end_owned())
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}|{}]", self.start, self.end)
    }
}
