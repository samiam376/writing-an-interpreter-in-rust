#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
}
