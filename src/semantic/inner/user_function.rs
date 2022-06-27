use std::cell::RefCell;
use std::rc::{Rc, Weak};

use crate::semantic::user_function;
use crate::InputSource;

pub type FinalUserFunction = user_function::UserFunction;
#[derive(Clone, Debug)]
pub struct UserFunction {
    pub name: Rc<str>,
    src: InputSource,
    me: Weak<Self>,
    //param_num: RefCell<Option<usize>>,
    //size: RefCell<FieldSize>,
    result: RefCell<Option<Rc<FinalUserFunction>>>,
}

impl UserFunction {
    pub fn new(name: &str, src: InputSource) -> Rc<Self> {
        Rc::new_cyclic(|me| Self {
            name: Rc::from(name),
            src,
            //size: RefCell::default(),
            //param_num: RefCell::default(),
            me: Weak::clone(me),
            result: RefCell::default(),
        })
    }
    pub fn name(&self) -> &Rc<str> {
        &self.name
    }
    pub fn me(&self) -> Rc<Self> {
        self.me.upgrade().unwrap()
    }
    //pub fn param_num(&self) -> Option<usize> {
    //    *self.param_num.borrow()
    //}
    //pub fn set_param_num(&self, value: usize) -> Result<bool, ExecutionError> {
    //    let current = *self.param_num.borrow();
    //    match (current, value) {
    //        (Some(current), new) if current != new => {
    //            return Err(ExecutionError::UserFunctionParamNumber(self.src.clone()))
    //        }
    //        (Some(_current), _new) /*if _current == _new*/ => Ok(false),
    //        (None, new) => {
    //            *self.param_num.borrow_mut() = Some(new);
    //            Ok(true)
    //        },
    //    }
    //}
    pub fn convert(&self) -> Rc<FinalUserFunction> {
        if self.result.borrow().is_none() {
            *self.result.borrow_mut() = Some(Rc::new(FinalUserFunction {
                name: Rc::clone(self.name()),
            }));
        }
        Rc::clone(&self.result.borrow().as_ref().unwrap())
    }
    //pub fn size(&self) -> FieldSize {
    //    *self.size.borrow()
    //}
    //pub fn update_size(&self, value: &FieldSize) -> Option<bool> {
    //    self.size.borrow_mut().update(&value)
    //}
}
