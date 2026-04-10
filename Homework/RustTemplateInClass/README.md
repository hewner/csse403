# In-Class Activity: Traits and Generics

The structs `Book` and `Movie` are defined in `src/main.rs`, along with a generic function
`print_bib` that can print a bibliography for any slice of items that implement
the `BibliographyEntry` trait.

## Part 1: A Basic Trait

1. Define the `BibliographyEntry` trait with a single method:
    ```rust
    fn to_bib_entry(&self) -> String;
    ```

2. Implement `BibliographyEntry` for `Book`. Here is the implementation:
    ```rust
        fn to_bib_entry(&self) -> String {
            format!(
                "{} ({}). {}. {}.",
                self.author, self.year, self.title, self.publisher
            )
        }
    ```

3. Implement `BibliographyEntry` for `Movie` using a similar format.

The code in `main` should compile and run correctly once you have completed the steps above.

## Part 2: External Types

The `url` crate is already in `Cargo.toml` and provides a `Url` type that represents a parsed URL.
Because `BibliographyEntry` is your own trait, you can implement it on `Url` directly — no wrapper needed.

4. At the top of `src/main.rs`, uncomment:
    ```rust
    use url::Url;
    ```

5. Implement `BibliographyEntry` for `Url`.

6. At the bottom of `main`, uncomment the `pages` vector and the `print_bib(&pages)` call to see it in action.
