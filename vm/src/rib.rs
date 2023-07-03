use crate::object::Object;

pub const FIELD_COUNT: usize = 3;

#[derive(Clone, Copy, Debug)]
pub struct Rib<'a> {
    fields: &'a [Object; FIELD_COUNT],
}

impl<'a> Rib<'a> {
    pub fn new(fields: &'a [Object; FIELD_COUNT]) -> Self {
        Self { fields }
    }

    pub fn car(&self) -> Object {
        self.fields[0]
    }

    pub fn cdr(&self) -> Object {
        self.fields[1]
    }

    pub fn tag(&self) -> Object {
        self.fields[2]
    }
}

#[derive(Debug)]
pub struct RibMut<'a> {
    fields: &'a mut [Object; FIELD_COUNT],
}

impl<'a> RibMut<'a> {
    pub fn new(fields: &'a mut [Object; FIELD_COUNT]) -> Self {
        Self { fields }
    }

    pub fn car_mut(self) -> &'a mut Object {
        &mut self.fields[0]
    }

    pub fn cdr_mut(self) -> &'a mut Object {
        &mut self.fields[1]
    }

    pub fn tag_mut(self) -> &'a mut Object {
        &mut self.fields[2]
    }
}
