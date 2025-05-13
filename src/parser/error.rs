#[derive(PartialEq)]
pub enum ParseError {
    Finished,
    Error(String),
}
