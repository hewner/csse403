
use rand::Rng;
use std::{thread, time};

fn do_race(racer_num : u32) {

    let time_to_wait = rand::thread_rng().gen_range(0..1000);
    thread::sleep(time::Duration::from_millis(time_to_wait));

    for checkpoint in 1..4 {
        println!("racer {} passed checkpoint {}", racer_num, checkpoint);
        let time_to_wait = rand::thread_rng().gen_range(0..1000);
        thread::sleep(time::Duration::from_millis(time_to_wait));
    }
    println!("racer {} finished!", racer_num);

}

fn main() {

    do_race(1);
    
}
