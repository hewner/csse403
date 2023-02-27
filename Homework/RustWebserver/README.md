# Rust Webserver

So for Rust we're going to build an application well within Rust's
strengths - a high performance Rust webserver, suitable for rendering
webpages or as a vehicle for a REST style webservice.  There are
several such webservers that already exist for Rust - in this case
we'll ask you to build in pure standard rust without any additional
libraries.

# Step 1: Multithreaded webserver (30 points)

If you look at [src/main.rs/](src/main.rs) you'll
see the start for a rust webserver.  Compiling and running with "cargo run"
should give you a program that will run a webserver on port 8080.  You
should be able to hit http://localhost:8080 with your browser (note:
some virtual machines may complicate that) and see it serve your a
hello world webpage.  You should also be able to say something like

    curl http://localhost:8080
    
And see a dump of the relevant HTML - though you may have to install
the curl utility to do so.

This webserver is insufficient for our purposes because it is single
threaded.  Instead, we would like to use the approach of a thread
pool.  When the server starts, it should create X threads, where X is
the size of the pool.  All these threads should attempt to use the
same TcpListener (well, actually a TcpListener cloned from the
original with the try\_clone() method).  When a web request comes in,
exactly one of these listening threads should be awakened.  It will
handle that individual request, while the other X - 1 threads wait for
other incoming connections.  If more than X threads arrive at the same
time, the rest will be queued up in the listener.  This queuing is
generally what we want - it prevents our webserver from spawning so
many threads it overwhelms the system.

Your http server should take a parameter indicating the number of
threads to start.

So what you should see on startup is something like this:

    $ ./http_server 3

    Listening for connections on port 8080
    Listening for connections on port 8080
    Listening for connections on port 8080

Even if the output is right though, it's easy to mess up the thread
behavior.  To check it, add a long sleep to the bottom of the
handle\_write function:

    let ten_secs = time::Duration::from_secs(10);
    thread::sleep(ten_secs);

Then repeatedly request pages from the server, I recommend like this:

    curl http://localhost:8080 &
    
What you should see is that if you initialized your server with 3
threads, the first 3 pages should show output quite fast.  Then the
4th page should take quite a while, because it needs to wait for one
of the first 3 threads to complete before it can process another.

If that works you've got a basic multithreaded webserver.

# Step 2: A "database" (30 points)

So webserver parallelism is all well and good, but most real
webservers actually have to return up-to-date data.  This tends to be
a problem, because often one user's requests can effect another user's
requests (e.g. if I bid on an ebay item, another later user's bid on
an ebay item may be affected).  Often this kind of issue is handled
through a shared database or webservice (or both) but to keep things
simple I have something called fortunes which reads fortunes from a
data file.  Here's how it works:

    let mut reader = FortuneReader::new().unwrap();
    println!("first fortune: {}", reader.next_fortune().unwrap());
    println!("second fortune: {}", reader.next_fortune().unwrap());

In this step, we want every webpage render to contain a fortune.

FortuneReader is our database, which reads classic unix fortunes from
a datafile in order until they run out and the whole program crashes.
To make this fortunes thing like a database, I'm going to require
that:

1.  We only open the datafile once
2.  We never reuse a fortune, so if we have multiple simultaneous
    requests each request must be given a unique fortune

Basically this amounts to requiring there only be 1 FortuneReader
object.  How can this be achived in Rust?  There are a couple ways
(and any that work are fine) I think the easiest is to wrap the
fortune reader in an Arc Mutex

    let reader = fortune::FortuneReader::new().unwrap();
    let data_mutex = Arc::new(Mutex::new(reader));

This makes a cloneable object that can be shared between the threads.
Before the fortune can be used, however, it must be locked.  Take a
look at the documentation for Arc and Mutex to see the details.

Be sure you actually make the webpage display the fortune you get!

## Step 3: Generics Based URL Matcher (40 points)

So we're going to do webserver related task here that actually won't
be needed in our little webserver.  I could have added it officially,
but I figured you'd rather get to the meat of the problem rather than
implementing a complex enough set of BS webserver features to justify
URL matching.

So webservers use URLs that sometimes correspond to files.  For
example if a see a URL like

http://www.mycoolwebsite.com/funstuff/video_games.html

It's probably not unliky that on that webserver there is a directory
called funstuff and in that directory there is a file called called
video_games.html, and that's what's being sent when you hit that URL.

But modern webservers (and especially REST webservices) often just use
the URL as a generic information passing mechanism.  For example, in
this URL

https://www.amazon.com/dp/1718500440

1718500440 is probably not a directory on any amazon server.  Most
likely "dp" represents a command (maybe something like item-lookup)
and 1718500440 represents a parameter to the command - some internal
amazon id number.

Different commands can of course have different parameters.  What this
usually amounts to that a webserver needs to take a URL & match the
various sub-parts of the URL - generating parameters that can be
passed to the functions that do the actual work.  At the same time,
the webserver needs to validate the URL - i.e. ensure it's formatted
as expected.  An invalid URL match might be we just aren't matching
against the right pattern (i.e. we might have a list of patterns and
we respond to the first command that matches successfully) or it could
mean a bogus URL that we need to error on.  Either way, the matching
has to consider the possibility that the URL does not match its
format.

All the code we're talking about here is in
[src/urlmatcher.rs](src/urlmatcher.rs).

### Our URL Matcher Trait

    pub trait UrlMatcher<T> {
        fn do_match<'a>(&self, s:&'a str) -> Option<(T, &'a str)>;
    }

So this trait is a little complicated but it's not really so bad.

A URL matcher is a thing that returns a parameter type from a str.
The type it returns is the T - some return strings, some integers and
of course as we progress it'll get more interesting than that.  But
for now it's pretty simple - look at the first example:

        let matcher = FixedWidthNum { width : 4 };
        let (a, b) = matcher.do_match("1234hello").unwrap();

        assert_eq!(a, 1234);
        assert_eq!(b, "hello");

FixedWidthNum is a matcher that returns a u64 - so it implements the
trait URLMatcher\<u64\>.

Matchers have to implement 1 function.  This function takes a string
as a parameter and it returns a Option pair.  The first element of the
pair is the parameter type (T) that its matches generate.  The second
element of the pair is the string that is left after the match -
i.e. a matcher will "eat" as much of the string as it can, but then it
will say what is left.  If we want, we can use another matcher on
that remaining string to get other parameters out of what's left.

If the match fails (e.g. you try to use a FixedWithNum matcher on some
alphabetic characters say) the matcher will return None - no
parameter, no remaining string.

### Basic Matcher 1: FixedWidthNum

Implement the FixedWidthNum matcher.  There is a test you can run with
cabal test called test\_fixed\_width\_num in urlmatcher.rs - you can
see the individual specifics there.

The fixed witch matcher is initialized with the length to use.

Basically, the FixedWidthNum matcher expects a nonnegative integer of
a specific character width at the beginning of the string.  The string
must always be at least the given length in size - otherwise the
matcher fails.  The string must have only digits in the given length -
otherwise the matcher fails.  To represent a number with less than the
given number of zeros, put zeros at the beginning (e.g. 00017 is 17
with length 5).

Test this code by running "cargo test" the test cases themselves are
at the bottom of urlmatcher.rs.

## Basic Matcher 2: AlphaMatcher

        let matcher = AlphaMatcher { };
        let (a, b) = matcher.do_match("hello1234").unwrap();
        assert_eq!(a, "hello");
        assert_eq!(b, "1234");


AlphaMatcher expects the matched string to begin with a series of
alphabetic characters.  It matches all alphabetic characters, then
leaves an non-alphabetic characters unmatched.  Alphabetic is defined
by Rust's is\_alphabetic character function.

AlphaMatcher considers it a match failure if at least one character is
not alphabetic.

*As you get started with this and other urlmatcher parts, be sure to
uncomment the relevent tests in urlmatcher!*


## Complex Matcher 1: StringAndThen

So this is a generic matcher that works with another existing matcher

    let matcher = StringAndThen::new("http://foo.com/".to_string() , AlphaMatcher { });
    let (a, b) = matcher.do_match("http://foo.com/hello1234").unwrap();
    assert_eq!(a, "hello");
    assert_eq!(b, "1234");

It basically allows matches to fixed strings that don't actually
contain parameter information, but do fail the match if they are not
present.

Hint: this matcher contains another matcher, which needs to be
parameterized with a type.  This parameter type can be a problem,
because its type isn't actually used as a field in StringAndThen in
any way.  Rust will start complaining that you should use something
called PhantomData.  While that can work, another option is just to
leave StringAndThen's matcher type unconstrained (i.e. it could
theoretically be a non-matcher type), but only implement the
UrlMatcher trait for the case when the matcher is actually a matcher
type.

## Complex Matcher 2: AggMatcher

So an aggmatcher is a "aggregate matcher" - a matcher that is a
combination of 2 other matchers.  It succeeds only if both of its
contained matchers succeed.  Its match is a tuple type.

    let matcher = FixedWidthNum { width : 4 };
    let matcher2 = AlphaMatcher { };
    let matcher3 = AggMatcher::new(matcher, matcher2);
    let ((a3, a4), b3) = matcher3.do_match("1234hello5").unwrap();
    assert_eq!(1234, a3);
    assert_eq!("hello", a4);
    assert_eq!("5", b3);

## We're done!

Hopefully you can see how these matchers can be recombined to parse
complicated urls like
http://foobar.com/product_id/1234/state_code/hello (if not, take a
look at the last test).  And as usual, aggregate matchers can contain
other aggregate matchers so we can parse any number of parameters in
our URLs with a single matcher.  If we wanted have some more fun, we
could make it even easier to create aggregate matchers using
operators, make things like String and tuples implement the matcher
trait, etc.  But this should be enough to give you a feel.

Turn in your main.rs and your urlmatcher.rs in the usual way.


