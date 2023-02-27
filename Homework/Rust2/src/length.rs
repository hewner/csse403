
pub trait Length {
    fn to_meters(&self) -> f32;
    fn to_string(&self) -> String;
    fn add(&self, b:&Self) -> Self;
}

pub struct MeterLength {    
    length_in_meters : f32
}


pub struct EnglishLength {
    length_in_inches : u32
}
