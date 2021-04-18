use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Val {
    Nil,
    Bool(bool),
    Int(i64),
    Real(f64),
    String(String),
    List(Rc<Val>, Rc<Val>),
    Tree(Rc<Val>, Rc<Val>, Rc<Val>),
    Function(usize),
    Error(Rc<Val>),
}

pub type Stack = Vec<Val>;

pub type BuiltinFn = fn(&mut Stack);

impl Val {
    pub fn add(self, other: Self) -> Val {
        todo!()
    }
}
