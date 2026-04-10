// See README.md for instructions.

// use url::Url;

struct Book {
    title: String,
    author: String,
    year: u32,
    publisher: String,
}

struct Movie {
    title: String,
    director: String,
    year: u32,
    studio: String,
}

// TODO: define BibliographyEntry trait here

// TODO: implement BibliographyEntry for Book here

// TODO: implement BibliographyEntry for Movie here

fn print_bib<T: BibliographyEntry>(entries: &[T]) {
    for entry in entries {
        println!("{}", entry.to_bib_entry());
    }
}

fn main() {
    let books = vec![
        Book {
            title: String::from("The Rust Programming Language"),
            author: String::from("Klabnik, S. & Nichols, C."),
            year: 2019,
            publisher: String::from("No Starch Press"),
        },
        Book {
            title: String::from("Programming Rust"),
            author: String::from("Blandy, J. & Orendorff, J."),
            year: 2021,
            publisher: String::from("O'Reilly Media"),
        },
    ];

    let movies = vec![
        Movie {
            title: String::from("Inception"),
            director: String::from("Nolan, C."),
            year: 2010,
            studio: String::from("Warner Bros."),
        },
        Movie {
            title: String::from("The Matrix"),
            director: String::from("Wachowski, L. & Wachowski, L."),
            year: 1999,
            studio: String::from("Warner Bros."),
        },
    ];

    println!("=== Books ===");
    print_bib(&books);

    println!("\n=== Movies ===");
    print_bib(&movies);

    // let pages = vec![
    //     Url::parse("https://www.rust-lang.org").unwrap(),
    //     Url::parse("https://doc.rust-lang.org/book/").unwrap(),
    // ];
    //
    // println!("\n=== Web Pages ===");
    // print_bib(&pages);
}

