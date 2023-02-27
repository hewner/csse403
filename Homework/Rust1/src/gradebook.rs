

use std::io::Write;
use std::collections::HashMap;

struct Student {
    name : String,
    grades : Vec<f64>
}


fn main() {


    let mut gradebook : HashMap<String, Student> = HashMap::new();
    
    loop {
        let mut input = String::new();
        print!("Gradebook> ");
        std::io::stdout().flush().unwrap();

        let _b1 = std::io::stdin().read_line(&mut input).unwrap();  

        // Print text to the console
        let mut parameters = input.split(" ");

        let command = parameters.next().unwrap().trim_end();

        println!("handling command \"{}\"\n", command);
        if command == "exit" {
             std::process::exit(0);
        }
        
        if command == "add-student" {
            let name = parameters.next().unwrap().trim_end().to_string();

            // TODO: create a function called add student and call it here
            // it will need to take both the gradebook and name as a parameter
            // in some form, but exactly how you want to do it is up to you
        }
        if command == "show-grades" {

        }
        if command == "add-grade" {
            let student_name = parameters.next().unwrap().to_string();
            let student_name2 = parameters.next().unwrap().to_string();
            let grade : f64 = parameters.next().unwrap().trim_end().parse().unwrap();
                
            
        }
    }
    
}
