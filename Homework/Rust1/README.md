# Compiling the code

This code is in a rust cargo project - rust's packaging system.

To compile the given code

    cargo build
    
To run a particular binary

    cargo run --bin gradebook
    cargo run --bin racing

# Rust Gradebook (20 points)

So in this assignment we're going to build a simple app that will let
us play just a bit with the borrow checker.  It's a little interactive
text application that lets us enter student grades.

Before you start, I would encourage you to read the sections on
ownership and borrowing here:

https://learning-rust.github.io/docs/c1.ownership.html

## Part 1: Add Student (10 points)

Take a look at the given student gradebook code.  You can see we've
already built most of the text processing for you.  The command we'll
add in this section is add student - it works like this:

    Gradebook> add-student A
    handling command "add-student"
    
    added student A to database
    
    Gradebook> add-student B
    handling command "add-student"
    
    added student B to database

You can see we've also already created a student struct type for
students and a HashMap called gradebook.  Your add student code should
simply create a new Student with the given name and an empty grade
list and insert it into the hashtable.

There is one additional wrinkle though - I would like you do this work
in a separate add-student method.  This will let you practice how
passing parameters works in Rust.  The code comment has some details.

## Part 2: Show Grades (3 points)

This should be fairly straightforward - add code for the show grades
command.  It works like this:

    Gradebook> add-student Alice
    handling command "add-student"
    
    added student Alice to database
    
    Gradebook> add-student Bill
    handling command "add-student"
    
    added student Bill to database
    
    Gradebook> show-grades
    handling command "show-grades"
    
    Alice - []
    Bill - []

Note that the grade lists are empty right now because students don't
have grades - but soon we will add that feature.

Small hint: if you want to print a vector of floats, use {:?} rather
than {} in your println!.

## Part 3: Add grade (7 points)

So the add grade function adds a grade.  There is a complication
though, in this class all grades are assigned for pairs of students.
So the add grade command expects 2 students.

    Gradebook> add-student Alice
    handling command "add-student"
    
    added student Alice to database
    
    Gradebook> add-student Bill
    handling command "add-student"
    
    added student Bill to database
    
    Gradebook> show-grades
    handling command "show-grades"
    
    Alice - []
    Bill - []
    Gradebook> add-grade Alice Bill 78
    handling command "add-grade"
    
    Gradebook> add-student Charles
    handling command "add-student"
    
    added student Charles to database
    
    Gradebook> add-grade Alice Charles 100
    handling command "add-grade"
    
    Gradebook> show-grades
    handling command "show-grades"
    
    Alice - [78.0, 100.0]
    Charles - [100.0]
    Bill - [78.0]
    
Note that this code will not work

    let s1 = gradebook.get_mut(&student_name).unwrap();
    let s2 = gradebook.get_mut(&student_name2).unwrap();
    s1.grades.push(grade);

Make sure you understand why before you try and fix it.

# Rust Racing Processes (20 points)

In this exercise I'd like you to experiment with rust threading.  Be
sure you watch the 3rd rust lecture before you attempt this.  Take a
look in racing.rs.  You will see a function do_race that has a thread
going through 3 checkpoints and then finishing.  Each checkpoint takes
a random amount of time to complete.  If you run the unmodified code,
you should see something like this:

    racer 1 passed checkpoint 1
    racer 1 passed checkpoint 2
    racer 1 passed checkpoint 3
    racer 1 finished!

## Part 1: Some threads (10 points)

Modify this code so it starts 3 threads the do the race in paralell.
When you run the code you should see something like this:

    racer 3 passed checkpoint 1
    racer 1 passed checkpoint 1
    racer 2 passed checkpoint 1
    racer 1 passed checkpoint 2
    racer 2 passed checkpoint 2
    racer 3 passed checkpoint 2
    racer 3 passed checkpoint 3
    racer 1 passed checkpoint 3
    racer 2 passed checkpoint 3
    racer 3 finished!
    racer 1 finished!
    racer 2 finished!

Note that the order should be different each time.  Further note that
you'll have to put a loop in your main which means your program will
not exit without being killed (we'll handle that in a graceful way in
the next part).

# Part 2: Communication (10 points)

Each of the racer threads should communicate back to the main thread
when they pass a checkpoint.  The main thread should keep track of the
furthest checkpoint reached and print *only* when some thread reaches
a checkpoint that hasn't been reached before.  When a thread finishes,
it should communicate a checkpoint 4 back to the main thread - which
the main thread should interpret as having finished the race and end
the program (hint: return will do the job).  It should look something
like this:

    racer 3 passed checkpoint 1
    a thread has passed checkpoint 1
    racer 2 passed checkpoint 1
    racer 2 passed checkpoint 2
    a thread has passed checkpoint 2
    racer 1 passed checkpoint 1
    racer 1 passed checkpoint 2
    racer 3 passed checkpoint 2
    racer 2 passed checkpoint 3
    a thread has passed checkpoint 3
    racer 1 passed checkpoint 3
    racer 2 finished!
    the race is over!

To accomplish this, create a channel that lets threads communicate
back to the main thread.  They only need to communicate integers (the
checkpoint reached) The details are here:

https://doc.rust-lang.org/book/ch16-02-message-passing.html

Note that you will have to modify both the parameters and the code of
the do-race function to do this.
