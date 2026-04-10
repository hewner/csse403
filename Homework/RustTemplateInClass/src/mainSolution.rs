use url::Url;

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

trait BibliographyEntry {
    fn to_bib_entry(&self) -> String;
}

impl BibliographyEntry for Book {
    fn to_bib_entry(&self) -> String {
        format!(
            "{} ({}). {}. {}.",
            self.author, self.year, self.title, self.publisher
        )
    }
}

impl BibliographyEntry for Movie {
    fn to_bib_entry(&self) -> String {
        format!(
            "{} (Director). ({}). {} [Film]. {}.",
            self.director, self.year, self.title, self.studio
        )
    }
}

impl BibliographyEntry for Url {
    fn to_bib_entry(&self) -> String {
        format!("Retrieved from {}", self)
    }
}

// Because BibliographyEntry is our trait, we can implement it for
// Box<dyn BibliographyEntry>. That lets print_bib (unchanged) accept
// a heterogeneous Vec<Box<dyn BibliographyEntry>>.
impl BibliographyEntry for Box<dyn BibliographyEntry> {
    fn to_bib_entry(&self) -> String {
        self.as_ref().to_bib_entry()
    }
}

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

    let pages = vec![
        Url::parse("https://www.rust-lang.org").unwrap(),
        Url::parse("https://doc.rust-lang.org/book/").unwrap(),
    ];

    println!("\n=== Web Pages ===");
    print_bib(&pages);

    // Heterogeneous list mixing all three types
    let mixed: Vec<Box<dyn BibliographyEntry>> = vec![
        Box::new(Book {
            title: String::from("The Rust Programming Language"),
            author: String::from("Klabnik, S. & Nichols, C."),
            year: 2019,
            publisher: String::from("No Starch Press"),
        }),
        Box::new(Movie {
            title: String::from("Inception"),
            director: String::from("Nolan, C."),
            year: 2010,
            studio: String::from("Warner Bros."),
        }),
        Box::new(Url::parse("https://www.rust-lang.org").unwrap()),
    ];

    println!("\n=== Mixed ===");
    print_bib(&mixed);
}
