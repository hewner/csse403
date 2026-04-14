use std::net::{TcpStream, TcpListener};
use std::io::{Read, Write};

mod fortune;
mod urlmatcher;
use urlmatcher::UrlMatcher;

// adapted from https://gist.github.com/mjohnsullivan/e5182707caf0a9dbdf2d

fn handle_read(mut stream: &TcpStream) -> Option<String> {
    let mut buf = [0u8 ;4096];
    match stream.read(&mut buf) {
        Ok(_) => {
            let req_str = String::from_utf8_lossy(&buf);
            println!("{}", req_str);
            // HTTP request line looks like: "GET /some/path HTTP/1.1"
            // We want the path, which is the second whitespace-delimited token
            let first_line = req_str.lines().next()?;
            let path = first_line.split_whitespace().nth(1)?;
            Some(path.to_string())
        },
        Err(e) => {
            println!("Unable to read stream: {}", e);
            None
        }
    }
}

fn handle_write(mut stream: TcpStream) {
    let response = b"HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n<html><body>Sure would be cool to have a fortune to display here!</body></html>\r\n";
    match stream.write(response) {
        Ok(_) => println!("Response sent"),
        Err(e) => println!("Failed sending response: {}", e),
    }
}

fn handle_client(stream: TcpStream) {
    let _path = handle_read(&stream);
    handle_write(stream);
}

fn main() {

    let listener = TcpListener::bind("127.0.0.1:8080").unwrap();
    println!("Listening for connections on port {}", 8080);

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                handle_client(stream);
            }
            Err(e) => {
                println!("Unable to connect: {}", e);
            }
        }
    }
}

// Step 4: Implement this function to match URLs and return content.
// Return None if the URL can't be matched (webserver should fall back to fortune).
fn compute_response(_path: &str) -> Option<String> {
    None // TODO: implement URL matching routes
}

// Step 4 tests

#[test]
fn test_contact_us() {
    let result = compute_response("/contact-us").unwrap();
    assert!(result.starts_with("Contact us"));
}

#[test]
fn test_calendar() {
    assert_eq!(compute_response("/calendar/04/2026"), Some("Calendar for April 2026".to_string()));
    assert_eq!(compute_response("/calendar/01/2000"), Some("Calendar for January 2000".to_string()));
    assert_eq!(compute_response("/calendar/12/1999"), Some("Calendar for December 1999".to_string()));
}

#[test]
fn test_no_match() {
    assert_eq!(compute_response("/"), None);
    assert_eq!(compute_response("/nonexistent"), None);
    assert_eq!(compute_response("/calendar/4/2026"), None);
}
