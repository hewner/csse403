use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;


pub struct FortuneReader {
    fortune_file : BufReader<File>
}

impl FortuneReader {

    pub fn new() -> std::io::Result<FortuneReader> {

        let file = File::open("fortunes.txt")?;
        println!("Opened the fortune file (this should only happen once!)");
        let buf_reader = BufReader::new(file);
        Ok ( FortuneReader { fortune_file : buf_reader } )
        
    }

    pub fn next_fortune(&mut self) -> std::io::Result<String> {
        let mut big_string = String::new();
        loop {
            let mut line = String::new();
            self.fortune_file.read_line(&mut line) ?;

            if line.starts_with("%") { return Ok(big_string); }
            big_string.push_str(line.as_str());
        }
    }

    
}

// fn main() {
//     let mut reader = FortuneReader::new().unwrap();
//     println!("{}", reader.next_fortune().unwrap());
//     println!("{}", reader.next_fortune().unwrap());
// }
