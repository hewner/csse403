# Rust Traits & Generics

So in this assignment we're going to explore the world of writing
flexible code through generics (also frequently called templates in
other languages).  This approach is something that can sometimes give
us flexibility on par with untyped languages like Python but with very
low cost in terms of performance and better safety.  It also has some
limitations and can get arcane.

# Part 1: Basic Traits (10 points)

Traits in Rust are like interfaces in Java (or typeclasses in
Haskell) - they are a lists of methods.  You can declare a particular
type supports a trait by implementing the methods.  You can do this
after the type is created, so if you want you can use Traits to add
new methods to existing types like String Vector etc. But the more
common use is to specify some common generic interface that a bunch of
types you will create will support.

If you look in length.rs you can see I've made a new trait Length

    pub trait Length {
        fn to_meters(&self) -> f32;
        fn to_string(&self) -> String;
        fn add(self, other:Self) -> Self;
    }

Length represents a linear length - but we might want to represent
that length in various ways (.e.g. regular meters, imperial inches and
feet).  Sometimes it might be useful to transform that length into
generic meters (hence the to_meters method), but other times we want
to keep it represented in its true form for printing and adding.

I've also provided you with 2 structs that should implement the Length
trait (plus an additional construction method new).  Take a look the
way the structs are used in the first 2 tests (use "cargo test" to run
them) and make the tests pass.

# Part 2: Generic Methods (10 points)

Thus far the traits haven't provided us any generality - let's fix
that.  Let's make a method is\_tall.  Is tall will take any type that
implements the Length trait, and returns true if the length
(presumably a human height in this case) is greater than 2 meters.

The signature of is\_tall will look like this

    pub fn is_tall<T:Length>(length : &T) -> bool
    
What we are saying here is that is tall is a generic method that will
work on any type T, so long as that type implements the length trait.

Go ahead and implement the is\_tall method and you should pass the
istall test (uncomment it in main.rs to see).  When you've got it
passing, continue on.

## Static Types

The is tall method might look similar to the equivalent of a method
like this in Java, where Length is some interface:

    bool isTall(Length l)
    
But there are some important differences.  The method in java is fully
generic when compiled - i.e. if you make a new class that implements
Length, you can pass it to isTall without even recompiling the isTall
code.

In Rust on the other hand, a new version of the isTall method is
compiled for each type that it is used with.  This makes things faster
when the code is run, because we can optimize each version for the
specific kind of type.  It also has some disadvantages - larger
binaries for instance but also the specific requirement that we able
to know the true type of anything we want to call isTall on.  Usually
this is no problem - sometimes this is hard.

## A Second Generic Function: triple

We want to write a function triple that takes a length and triples it.
We want this to be a generic function - i.e. not have to implement a
separate version for each type.  This might seem to impossible given
what we have but it's doable (mostly) - tripling is just adding
something to itself twice.

But there is one challenge - given that add takes ownership of its
parameters, we can't call add with the same input twice - unless we
can copy it.  So this function will only work with lengths that are
also copyable - actually we can get away with just cloable -
i.e. having a clone method that does the copy.  Implement this
function (hint, you'll need to explicitly call the method clone).
Here's the signature:

    pub fn triple<T:Length + Clone >(length : T)
    
Note the flexibility here: we don't have to require that every Length
be clonable.  Instead, for lengths that are clonable you can use the
method triple - for others, you can use other methods but not triple.

But how to we make Lengths clonable?  One way would be to implement
the Clone trait.  But Clone is a one of the special traits that rust
can implement for us for simple structures.  Here's how it looks:

    #[derive(Copy, Clone)]
    pub struct EnglishLength {
        length_in_inches : u32
    }

Once you implement triple and make the lengths clonable you should be
able to uncomment and run the test\_triple.

# Generics and Aggregation (10 points)

One thing we can use generics for is for stuff like typed Vectors.  We
can make a templated structure that takes a type parameter, and now we
can have typed lists where the method signature reflects what's being
stored (e.g. if you declare a Vec\<bool\> the the get functions return
a bool, the set functions take a bool, etc.).

But a more interesting this we can do with generics is make compound
flexible types.

To show you how this works, let's make a new Length type called
LengthPair.  It consists of a pair or lengths and can be of different
types.  So I could have a LengthPair<MeterLength,EnglishLength>.  If I
want I can even have a
LengthPair<MeterLength,LengthPair<MeterLength,EnglishLength>>.  Here's
how to get started:

    #[derive(Copy, Clone)]
    pub struct LengthPair<A:Length,B:Length> {
        first : A,
        second : B
    }
    
    impl<A:Length,B:Length> LengthPair<A,B>{
        pub fn new(first:A, second:B) -> LengthPair<A,B>{
            LengthPair { first , second }
        }
    }

Finish LengthPair's implementation and make the test\_length\_pair and
test\_length_pair triple pass.

# True Runtime Polymorphism (5 points)

This last step is maybe a bit complicated for the points you get, but
we get to dive into some cooler Rust features.

So it might seem the LengthPair will allow us to have length chains of
any arbitrary size - it will, but only if the size can be determined
at compile time.

If we want something as simple a loop that constructs a really big
LengthPair - you'll discover it won't work.  For example:

    let mut var = EnglishLength::new(0,1);
    
    for q in 1..10 {
        let otherlength = EnglishLength::new(0,1);
        var = LengthPair::new(var, otherlength)
    }

The reason this won't work might more obvious if you consider: what is
the type of the variable var?

Sometimes types can not be determined at compile time.  We might want
to construct length objects based on strings loaded from a datafile.
We might want a list of lengths that are created randomly of either
type.  This kind of thing is easy to in Java - just create a function
or struct of the interface type and now it works for anything.  In
rust though we have a problem - because the type varies when the
program runs, we can't just use templates to compile version of the
function with the type explicitly known.

Rust can do what Java does for all its methods.  The trick is
something called a vtable (virtual table) where what looks like a
function call is actually based on a pointer stored in the object.  In
rust thing can only be done using Box - rusts little used pointer type
and the dyn attribute.

See how it's used here:

https://doc.rust-lang.org/stable/rust-by-example/trait/dyn.html

Doing so has some complications however.

Let's say we'd like to create a structure LengthList that is a Length
but contains a list of potentially different runtime determined lengths:

    pub struct LengthList {
        lengths : Vec<Box<dyn Length>>
    }

We will discover the the add function in the Length trait means it
isn't suitable for being dynamic.  It may require a minute of
reflection to see why - consider the return type of calling add on a
single element from that array of Lengths.


Solution?  Let's remove the add method from our Length trait and into
its own trait.  We could define our own Add trait, but you know what
would be cooler?  Using Rust's built in Add trait that would let us
get added with + if we wanted to.

https://doc.rust-lang.org/std/ops/trait.Add.html

Make all the various traits implement Add.  Hint: here's my impl for Add in LengthPair:

    impl<A:Length + Add<Output=A>,B:Length + Add<Output=B>> Add for LengthPair<A,B> {

If you do it correctly it should be possible implement LengthList as
described above and make all the tests work again, this time using add
from the new trait.  It should also be possible to uncomment and run
test\_add\_with\_plus and test\_length\_list.

# That's it!

Submit as usual!  I hope you found this interesting!
