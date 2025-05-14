#[derive(PartialEq, Debug)]
pub enum ParseError {
    Finished,
    Error(String),
}
