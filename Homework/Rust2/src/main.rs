use crate::length::*;
use std::ops::Add;

mod length;

fn main() {
    println!("Use cargo test not cargo run!");
}

#[test]
fn test_basic_meters() {
    let ml = MeterLength::new(3.0);
    let ml2 = MeterLength::new(5.0);
    assert_eq!(ml.to_meters(), 3.0);
    assert_eq!(ml.to_string(), "3 meters".to_string());
    assert_eq!(ml.add(ml2).to_meters(), 8.0);
}

#[test]
fn test_basic_english() {
    let el = EnglishLength::new(1, 2);
    let el2 = EnglishLength::new(0, 11);
    assert_eq!(el.to_meters(), 0.3556); // hint: an inch is 0.0254 meters
    assert_eq!(el.to_string(), "1 feet 2 inches".to_string());
    assert_eq!(el.add(el2).to_string(), "2 feet 1 inches".to_string());
}

// #[test]
// fn test_is_tall() {
//     let ml1 = MeterLength::new(2.0);
//     let ml2 = MeterLength::new(2.1);
//     let el1 = EnglishLength::new(6, 0);
//     let el2 = EnglishLength::new(7, 0);
//     assert_eq!(is_tall(&ml1), false);
//     assert_eq!(is_tall(&ml2), true);
//     assert_eq!(is_tall(&el1), false);
//     assert_eq!(is_tall(&el2), true);
// }


// #[test]
// fn test_triple() {
//     let ml = MeterLength::new(5.0);
//     let el = EnglishLength::new(0, 5);
//     assert_eq!(triple(ml).to_string(), "15 meters".to_string());
//     assert_eq!(triple(el).to_string(), "1 feet 3 inches".to_string());
// }

// #[test]
// fn test_length_pair() {
    
//     let ml = MeterLength::new(5.0);
//     let el = EnglishLength::new(0, 5);
//     let lp1 = LengthPair::new(ml,el);
    
//     assert_eq!(lp1.to_string(), "5 meters AND THEN 0 feet 5 inches".to_string());


//     let ml2 = MeterLength::new(5.0);
//     let el2 = EnglishLength::new(0, 5);
//     let lp2 = LengthPair::new(ml2,el2);

//     let add_result = lp1.add(lp2);
    
//     assert_eq!(add_result.to_string(), "10 meters AND THEN 0 feet 10 inches".to_string());

//     let ml3 = MeterLength::new(5.0);
//     let recursive_lp = LengthPair::new(lp1, ml3);
    
//     assert_eq!(recursive_lp.to_string(), "5 meters AND THEN 0 feet 5 inches AND THEN 5 meters".to_string());
    
// }

// #[test]
// fn test_length_pair_triple() {
    
//     let ml = MeterLength::new(5.0);
//     let el = EnglishLength::new(0, 5);
//     let lp1 = LengthPair::new(ml,el);
    
//     assert_eq!(triple(lp1).to_string(), "15 meters AND THEN 1 feet 3 inches".to_string());
// }

// #[test]
// fn test_add_with_plus() {
//     let ml = MeterLength::new(3.0);
//     let ml2 = MeterLength::new(5.0);
//     assert_eq!((ml+ ml2).to_meters(), 8.0);
// }


// #[test]
// fn test_length_list() {
    
//     let ml = MeterLength::new(5.0);
//     let el = EnglishLength::new(0, 5);
//     let mut ll1 = LengthList::new();

//     ll1.add_length(ml);
//     ll1.add_length(el);


//     assert_eq!(ll1.to_string(), "5 meters AND THEN 0 feet 5 inches".to_string());

// }

