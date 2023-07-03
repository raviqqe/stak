use crate::value::Value;

pub const FIELD_COUNT: usize = 3;

#[derive(Clone, Copy, Debug)]
pub struct Rib<'a> {
    fields: &'a [Value; FIELD_COUNT],
}

impl<'a> Rib<'a> {
    pub fn new(fields: &'a [Value; FIELD_COUNT]) -> Self {
        Self { fields }
    }

    pub fn car(&self) -> Value {
        self.fields[0]
    }

    pub fn cdr(&self) -> Value {
        self.fields[1]
    }

    pub fn tag(&self) -> Value {
        self.fields[2]
    }
}

#[derive(Debug)]
pub struct RibMut<'a> {
    fields: &'a mut [Value; FIELD_COUNT],
}

impl<'a> RibMut<'a> {
    pub fn new(fields: &'a mut [Value; FIELD_COUNT]) -> Self {
        Self { fields }
    }

    pub fn car_mut(self) -> &'a mut Value {
        &mut self.fields[0]
    }

    pub fn cdr_mut(self) -> &'a mut Value {
        &mut self.fields[1]
    }

    pub fn tag_mut(self) -> &'a mut Value {
        &mut self.fields[2]
    }
}
