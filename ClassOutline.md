<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Introduction](#introduction)
    - [Introduction](#introduction-1)
        - [Let's name some programming languages](#lets-name-some-programming-languages)
        - [Let's name some adjectives for programming languages](#lets-name-some-adjectives-for-programming-languages)
            - [My Adjective List](#my-adjective-list)
        - [Why so many?](#why-so-many)
            - [My answer](#my-answer)
        - [How is this class useful](#how-is-this-class-useful)
        - [We'll do some weird languages in this class](#well-do-some-weird-languages-in-this-class)
    - [Some important details](#some-important-details)
        - [Call me Buffalo](#call-me-buffalo)
        - [Design of this class](#design-of-this-class)
        - [Course Policies](#course-policies)
        - [Two stages in our learning](#two-stages-in-our-learning)
    - [What you need to do today](#what-you-need-to-do-today)
- [Prolog 1](#prolog-1)
    - [Facts and implications](#facts-and-implications)
        - [Let's see it in action!](#lets-see-it-in-action)
        - [How does it work?  Unification](#how-does-it-work--unification)
        - [Representation Activity](#representation-activity)
        - [Note that a single prolog function can be evaluated multiple ways](#note-that-a-single-prolog-function-can-be-evaluated-multiple-ways)
    - [Let's do some examples with lists](#lets-do-some-examples-with-lists)
        - [replace in list - replaces one value with another](#replace-in-list---replaces-one-value-with-another)
        - [Solution](#solution)
        - [is a member - is a particular value in a list](#is-a-member---is-a-particular-value-in-a-list)
        - [Solution](#solution-1)
        - [duplicate members - take a list and duplicate all its elements](#duplicate-members---take-a-list-and-duplicate-all-its-elements)
        - [Solution](#solution-2)
        - [only repeats - true if a list just contains the same element over and over](#only-repeats---true-if-a-list-just-contains-the-same-element-over-and-over)
        - [Solution](#solution-3)
- [Prolog 2](#prolog-2)
    - [Never Not an Unbound Variable](#never-not-an-unbound-variable)
    - [Use is for calculations](#use-is-for-calculations)
    - [Strings](#strings)
        - [Not necessarily consistent!  Last year's way (not true anymore)](#not-necessarily-consistent--last-years-way-not-true-anymore)
        - [I reccommend: always single quotes and atom\_chars](#i-reccommend-always-single-quotes-and-atom_chars)
    - [Cuts](#cuts)
        - [Challenge:](#challenge)
        - [Now lets read about cuts](#now-lets-read-about-cuts)
        - [Cuts in your homework](#cuts-in-your-homework)
    - [Append](#append)
    - [Work on Word Find HW](#work-on-word-find-hw)
- [Prolog 3 - a bit on user input](#prolog-3---a-bit-on-user-input)
    - [What Makes Prolog Good?](#what-makes-prolog-good)
    - [Prolog Input Basics](#prolog-input-basics)
    - [Prolog input challenge](#prolog-input-challenge)
        - [Write a function that takes in a prolog string (terminated with '\n')](#write-a-function-that-takes-in-a-prolog-string-terminated-with-n)
        - [Write a function that takes in a string, and returns a list of strings separated by spaces](#write-a-function-that-takes-in-a-string-and-returns-a-list-of-strings-separated-by-spaces)
- [Prolog 4 - Parsing](#prolog-4---parsing)
    - [Your Prolog Project](#your-prolog-project)
    - [Issue: Parses with variable length](#issue-parses-with-variable-length)
        - [Solution with some problems](#solution-with-some-problems)
        - [A more efficient but stranger solution](#a-more-efficient-but-stranger-solution)
        - [A specialized syntax for the stranger solution](#a-specialized-syntax-for-the-stranger-solution)
        - [I recommend you use the basic syntax, but it's up to you](#i-recommend-you-use-the-basic-syntax-but-its-up-to-you)
    - [Issue: Number Agreement](#issue-number-agreement)
        - [Solution](#solution-4)
    - [Issue: We want to output something](#issue-we-want-to-output-something)
        - [Think about it before you peek!](#think-about-it-before-you-peek)
- [More Useful Stuff](#more-useful-stuff)
    - [Compounds](#compounds)
    - [Debugging prolog](#debugging-prolog)
    - [A Few other details](#a-few-other-details)
- [Prolog Metafunctions](#prolog-metafunctions)
    - [Assert and retract](#assert-and-retract)
    - [Clause and call](#clause-and-call)
    - [Operators](#operators)
- [Rust 1](#rust-1)
    - [Very Basics](#very-basics)
    - [Variables & Ownership](#variables--ownership)
        - [First detail: variables immutable by default](#first-detail-variables-immutable-by-default)
        - [Variables are freed automatically](#variables-are-freed-automatically)
    - [Moves](#moves)
    - [Moves and functions](#moves-and-functions)
    - [Borrows](#borrows)
        - [Limits on Mutable Borrows](#limits-on-mutable-borrows)
        - [A little example](#a-little-example)
- [Rust 2 - More Memory & Errors](#rust-2---more-memory--errors)
    - [Borrows and dangling references](#borrows-and-dangling-references)
        - [Borrows have "lifetimes" associated with them](#borrows-have-lifetimes-associated-with-them)
        - [Lifetimes are complicated once we introduce functions that return references](#lifetimes-are-complicated-once-we-introduce-functions-that-return-references)
        - [Returned references also extend the length of borrows](#returned-references-also-extend-the-length-of-borrows)
        - [You probably do not want to use borrows to make linked structures](#you-probably-do-not-want-to-use-borrows-to-make-linked-structures)
        - [General advice](#general-advice)
    - [Errors and safety](#errors-and-safety)
        - [Option Type](#option-type)
        - [Propagating errors](#propagating-errors)
- [Rust 3: Threads](#rust-3-threads)
    - [Your basic Rust multi-threaded program](#your-basic-rust-multi-threaded-program)
    - [Passing data through move](#passing-data-through-move)
    - [Communicating with Messages](#communicating-with-messages)
- [Rust 4 - Objects & Generics](#rust-4---objects--generics)
    - [The Pre-basics](#the-pre-basics)
    - [Now the interesting stuff - Generic Functions](#now-the-interesting-stuff---generic-functions)
    - [Generic Structs](#generic-structs)
    - [Generic Structs with methods](#generic-structs-with-methods)
    - [Traits](#traits)
    - [Default implementations](#default-implementations)
    - [Traits in functions](#traits-in-functions)
    - [Fixing our largest example from the beginning](#fixing-our-largest-example-from-the-beginning)
- [Rust Debugging](#rust-debugging)
- [Haskell 1](#haskell-1)
    - [Pure Functional](#pure-functional)
    - [Has Strong Typing](#has-strong-typing)
        - [But also has type inference](#but-also-has-type-inference)
        - [Functions are designed for partial evaluation](#functions-are-designed-for-partial-evaluation)
        - [Activity](#activity)
        - [Solution](#solution-5)
    - [Pointfree style](#pointfree-style)
        - [$](#)
        - [.](#)
        - [Omitting parameters](#omitting-parameters)
    - [Hakell's IO](#hakells-io)
        - [But more importantly, how do you handle INPUT and State in a pure functional language?](#but-more-importantly-how-do-you-handle-input-and-state-in-a-pure-functional-language)
        - [Very basics](#very-basics)
        - [A function that does IO](#a-function-that-does-io)
- [Haskell 2](#haskell-2)
    - [Algebraic Data Types](#algebraic-data-types)
    - [Record Types](#record-types)
    - [Typeclasses](#typeclasses)
        - [Type classes can have default implementations](#type-classes-can-have-default-implementations)
        - [Type classes can have subclasses](#type-classes-can-have-subclasses)
        - [Polymorphism but not inheritance](#polymorphism-but-not-inheritance)
    - [Typeclasses can get very meta](#typeclasses-can-get-very-meta)
    - [Functors](#functors)
    - [Applicative Functors](#applicative-functors)
        - [My final running example](#my-final-running-example)
- [Elm 4 - Functional Design](#elm-4---functional-design)
    - [What is the point of design?](#what-is-the-point-of-design)
        - [The programs we want to write are complex, how can this be accomplished?](#the-programs-we-want-to-write-are-complex-how-can-this-be-accomplished)
    - [An Initial Example](#an-initial-example)
        - [Some variations](#some-variations)
    - [Functional folks love this!](#functional-folks-love-this)
        - [What is good about it?](#what-is-good-about-it)
        - [What is bad about it?](#what-is-bad-about-it)
    - [Let's talk about (idealized) OO paradigm](#lets-talk-about-idealized-oo-paradigm)
        - [What is good about it?](#what-is-good-about-it-1)
        - [What is bad about it](#what-is-bad-about-it)
    - [My advice:](#my-advice)
    - [Polymorphism](#polymorphism)
- [Haskell: Representing Type & State](#haskell-representing-type--state)
    - [OO Programming and "Types"](#oo-programming-and-types)
    - [In functional approaches we don't want too many types](#in-functional-approaches-we-dont-want-too-many-types)
    - [Acting over time](#acting-over-time)
- [The final project](#the-final-project)
- [Instructor's Choice: Smalltalk 1](#instructors-choice-smalltalk-1)
- [Instructor's Choice: Smalltalk, The Image](#instructors-choice-smalltalk-the-image)
    - [How can we be more object-oriented?](#how-can-we-be-more-object-oriented)
    - [What's going on with Smalltak](#whats-going-on-with-smalltak)
    - [What is the image?](#what-is-the-image)
        - [Sounds neat, but not really that important](#sounds-neat-but-not-really-that-important)
        - [This seems crazy](#this-seems-crazy)
        - [How do you create an image?](#how-do-you-create-an-image)
        - [How do you share code if it's trapped in an image?](#how-do-you-share-code-if-its-trapped-in-an-image)
    - [Why is an image good?](#why-is-an-image-good)
        - [I like files!  You wouldn't believe how good I am at vi.  Plus all my tools like git operate on files.](#i-like-files--you-wouldnt-believe-how-good-i-am-at-vi--plus-all-my-tools-like-git-operate-on-files)
        - [An example](#an-example)
        - [A programmatic programming environment gives you greater power](#a-programmatic-programming-environment-gives-you-greater-power)
    - [Some examples](#some-examples)
        - [A demo](#a-demo)
        - [The refactoring browser](#the-refactoring-browser)
- [Instructor's Choice: Haskell and Monads](#instructors-choice-haskell-and-monads)
    - [The idea](#the-idea)
        - [Oftentimes we have "almost" pure functions](#oftentimes-we-have-almost-pure-functions)
        - [Of course, we can always add return values!](#of-course-we-can-always-add-return-values)
        - [But the problem is nobody has time for that!](#but-the-problem-is-nobody-has-time-for-that)
        - [Why does this seem annoying?](#why-does-this-seem-annoying)
        - [But the context is screwing us up!](#but-the-context-is-screwing-us-up)
        - [Here's what (we think) we want!](#heres-what-we-think-we-want)
        - [How is this different then applicative functors?](#how-is-this-different-then-applicative-functors)
    - [Monads](#monads)
        - [Monadic type = value with some context](#monadic-type--value-with-some-context)
        - [Let's combine the functions with some crazy operator!](#lets-combine-the-functions-with-some-crazy-operator)
        - [Except what if we needed to use the value in a subsequent step?](#except-what-if-we-needed-to-use-the-value-in-a-subsequent-step)
        - [In Haskell, this can be improved with some syntactic sugar](#in-haskell-this-can-be-improved-with-some-syntactic-sugar)
    - [The elm maybe monad](#the-elm-maybe-monad)
    - [Write your own monads!](#write-your-own-monads)
- [Instructor's Choice 2: More on Monads](#instructors-choice-2-more-on-monads)
    - [Extended example](#extended-example)
        - [Step 1](#step-1)
        - [Step 2: add Monadic Type](#step-2-add-monadic-type)
        - [Step 3: functions should return monadic type](#step-3-functions-should-return-monadic-type)
        - [Step 4: let's make our combination operator](#step-4-lets-make-our-combination-operator)
        - [Step 5: Use it!](#step-5-use-it)
        - [Step 6: But divs](#step-6-but-divs)
        - [Step 7: Update our monadic type](#step-7-update-our-monadic-type)
        - [Step 8: Solving the problem of adding](#step-8-solving-the-problem-of-adding)
        - [Step 9: Conversion function is easy](#step-9-conversion-function-is-easy)
        - [Step 10: Sugar & we try it out](#step-10-sugar--we-try-it-out)
        - [Final version](#final-version)
    - [More examples](#more-examples)
        - [An Example: The Maybe Monad](#an-example-the-maybe-monad)
        - [Doing IO with Monads](#doing-io-with-monads)
        - [Many other Monads](#many-other-monads)
- [Buffalo's Guide to a Good Presentation](#buffalos-guide-to-a-good-presentation)
    - [Start with the activity in mind](#start-with-the-activity-in-mind)
    - [Learning itself is fun](#learning-itself-is-fun)
    - [Provide Feedback](#provide-feedback)
    - [Explore one topic deeply](#explore-one-topic-deeply)
    - [Things take time](#things-take-time)
    - [Check the rubric in the final project document](#check-the-rubric-in-the-final-project-document)
- [Instructor's Choice: Lua and C Integration](#instructors-choice-lua-and-c-integration)
    - [But probably its neatest feature is that is designed to be embedded](#but-probably-its-neatest-feature-is-that-is-designed-to-be-embedded)
        - [An Example](#an-example)
        - [Communication between lua and C is stack based](#communication-between-lua-and-c-is-stack-based)
    - [Why do we want an embedded programming language?](#why-do-we-want-an-embedded-programming-language)
    - [Now you try](#now-you-try)
        - [Installing Lua + C integration](#installing-lua--c-integration)
        - [Making sure it works](#making-sure-it-works)
        - [The Paper Scissors Rock Competition Activity](#the-paper-scissors-rock-competition-activity)
- [Instructor's Choice 6: Lua and Prototype Based OO](#instructors-choice-6-lua-and-prototype-based-oo)
    - [What is Object Oriented Programming?](#what-is-object-oriented-programming)
        - [Encapsulation](#encapsulation)
        - [Polymorphism](#polymorphism-1)
        - [Inheritance](#inheritance)
    - [Is there any way to make OO stuff simple for dynamic languages?](#is-there-any-way-to-make-oo-stuff-simple-for-dynamic-languages)
        - [Very crude solution](#very-crude-solution)
    - [Solving Duplication/Superclass modification](#solving-duplicationsuperclass-modification)
        - [How does this work](#how-does-this-work)
    - [Solving shallow copy](#solving-shallow-copy)
- [Instructor's Choice 6: More Prototype Based OO](#instructors-choice-6-more-prototype-based-oo)
    - [Prototypes in Self](#prototypes-in-self)
    - [How to implement the basics](#how-to-implement-the-basics)
        - [Key insight: it needs to be possible the modify the class later](#key-insight-it-needs-to-be-possible-the-modify-the-class-later)
        - ["Class" vs "Instance"](#class-vs-instance)
        - [Inheritance is trait object parents](#inheritance-is-trait-object-parents)
        - [What if you want multiple representations?](#what-if-you-want-multiple-representations)
    - [Dynamic Inheritance](#dynamic-inheritance)
        - [Prototype Solution](#prototype-solution)
        - [What do you think?](#what-do-you-think)
    - [Are prototypes a better abstraction than classes?](#are-prototypes-a-better-abstraction-than-classes)
    - [Using Objects to Store and Categorize](#using-objects-to-store-and-categorize)
- [Final Reflections](#final-reflections)
    - [Course Evaluations](#course-evaluations)
    - [The Languages](#the-languages)
        - [Prolog](#prolog)
        - [Erlang](#erlang)
        - [Elm](#elm)
        - [Smalltalk](#smalltalk)
        - [Lua](#lua)
        - [Your Project Language](#your-project-language)
    - [A Reminder of some of the goals in this class](#a-reminder-of-some-of-the-goals-in-this-class)
        - [The Paradigm](#the-paradigm)
        - [Skills in acquiring new programming languages](#skills-in-acquiring-new-programming-languages)
    - [Remember: paradigm rather than language](#remember-paradigm-rather-than-language)
        - ['Exciting' is a great adjective for a programming language sometimes](#exciting-is-a-great-adjective-for-a-programming-language-sometimes)
        - [Love the boring languages too!](#love-the-boring-languages-too)
    - [Don't let your exploration end here!](#dont-let-your-exploration-end-here)

<!-- markdown-toc end -->


# Introduction

> A language that doesn't affect the way you think about programming is not worth knowing.”
>      ― Alan J. Perlis (Turing Award Winner, developer of ALGOL)

## Introduction

### Let's name some programming languages

(On board)

### Let's name some adjectives for programming languages

#### My Adjective List

1. Object--Oriented/Functional/Procedural/Imperative
2. High Level/Low Level
3. Compiled, virtual machined, interpreted
4. Strongly typed/weakly typed
5. C--like (scheme--like, Smalltalk--like, etc.)
6. Scripting
7. Turing complete






### Why so many?

![Big Graph of Programming Languages](languages.png)

#### My answer

* Often languages are specialized to particular purposes (e.g. Php & web stuff)
* Or have technical differences that make them function in
  fundamentally different ways (e.g. compilation, garbage collection)

But there's not really enough differences of this sort to account for
the vast variation we see out there

I often feel that programming languages are artwork that programmers
make for other programmers, about what programming ought to be like.

* What is important
* What is irrelevant
* What problems are central, and what solutions do we agree work well
* What is fun
* What is not fun but you're a bad person if you don't pay attention
  to it

### How is this class useful

1. We're going to train you to be good at learning programming
   languages, which is a useful thing
2. The ideas of a programming language can be used in other languages
   (e.g. immutability of state)
3. I want you to have fun and enjoy programming in different ways, and
   spark your interest in continuing to learn to program in different
   ways
   
What you won't learn in this class

1. A lot of languages you'll directly use on projects in industry
2. A lot of strong opinions about what features languages ought to
   have or what programmers ought to do or like

### We'll do some weird languages in this class

-   Prolog
-   Erlang
-   Elm

## Some important details

### Call me Buffalo

Or Dr. Buffalo in a pinch
Or whatever you want

Check out Moodle for every imaginable kind of contact info (yes we can be facebook friends if you want but I don't post about classes).  You can call my cell phone if you're having a problem.

### Design of this class

Always a dream of mine to teach a class like this

1.  To teach you about some weird languages
2.  Why weird?
3.  To gain you practice and confidence learning languages on your own
4.  The process will involve a lot of google and a lot of coding
5.  It won't (for the most part) involve a lot of lecturing
6.  I am not an "expert" in the languages we will learn

### Course Policies

You are obligated to read and understand the complete Course Policies document.

It is on Moodle.

I will touch on the highlights only.

### Two stages in our learning

1.  One week of (reasonably intense) practice with a new language
2.  One week of work on a larger project

## What you need to do today

1.  Maybe install a unix environment on your laptop (recommended - you're responsible for your own environment)
2.  Install swi-prolog (link in the online quiz)
3.  Read chapter 4.1 and try out some of the examples
4.  Fill out the online quiz
5.  If you have time, feel free to look at the coding project due soon

# Prolog 1

## Facts and implications

In prolog, you have a knowledge base of things that are facts:

    food_type(cheddar,cheese). % Cheddar is a type of cheese
    food_type(swiss,cheese). % Swiss is a type of cheese
    food_type(oreo,cookie). % oreo is a type of cookie
    
    flavor(sweet,cookie). % cookies are sweet
    flavor(savory,cheese). % cheese is savory

And then you have implications:

    food_flavor(FoodName,Flavor) :- food_type(FoodName,Type) , flavor(Flavor,Type).
    % if a food is of some type, and that type has a flavor, then the food has the flavor

### Let's see it in action!

1.  Buffalo's in class example notes

        likes(buffalo, ninjas).
        likes(buffalo, videogames).
        % do an example with likes prolog filling in stuff
        
        % get an example from the class
        % find somebody who also likes video games
        likes(steveo, running).
        likes(alice, videogames).
        
        % then make a implication
        likes(buffalo,X) :- likes(X,videogames).
        likes(alice,X) :- likes(X,videogames), likes(X,ninjas).
        % how does this work?  Unification.
        
        example(ninja,pirate,robot).
        example(zombie,dino,alien).
        example(X,Y,Z) :- example(Y,X,Z). % note that this causes an infinite loop

### How does it work?  Unification

consider likes(alice, A).
to answer this we try and unify each likes predicate in order
likes(buffalo, ninjas) NOPE A can unify with ninjas but alice cant unify with buffalo
likes(buffalo, ninjas) NOPE same as above
likes(steveo, running) NOPE same as above
likes(alice, videogames) YES A can unify with videogames, alice unifies with alice.  So we return that.

If a continue happens we continue from where we left off
likes(buffalo, X) A can unify with X but buffalo can't unify with alice
likes(alice,X) this can unify so X and A unify and we look at the subpredicate
likes(A/X, videogames).  

We're going to try to unify this with all likes predicates starting
back at the beginning.  It will unify with likes(buffalo, videogames).
That unification will set A/X to buffalo

Continuing on with the likes(alice,X) predicate we will try and unify
likes(buffalo, videogames).  At this point all values have been set
but we still have to look it up.  It will unify with likes(buffalo,
videogames).  Nothing else needs to be satisfied so we return this
result.

If a continue happens we continue on as if likes(alice,buffalo) had
not unified.  So we're still in the likes(alice, X) predicate.  We'll
try likes(alice, steveo) but that won't work.

We'll try likes(alice, alice).  The alice videogames predicate will
work similarly to before.  But interestingly trying to see if alice
likes ninjas will call the likes(alice,X) predicate again.  So we'll
check if ninjas like videogames.  Because ninjas don't like videogames
alice doesn't like ninjas and so alice doesn't like herself.




### Representation Activity

Adapted from Programming in Prolog, Clocksin & Mellish 5th ed.

    % Suppose someone has already defined the following prolog relationships:
    
    male(X) /* x is male */
    female(X) /* x is female */
    parent(X,Y) /* X is a parent of Y */
    dif(X,Y) /* X and Y are different - this one is built in*/
    
    % Write prolog code to define the following other relationships
    
    is_mother(X) /* for people who are mothers */
    grandpa_of(X,Y) /* X is a granfather of Y */
    half_sister_of(X,Y) :- /* X is a half-sister of Y */
    
    % Try it out in your prolog intepreter and make sure it works!

### Note that a single prolog function can be evaluated multiple ways

    append([a],[b],X). % like a normal function
    
    ?- append(A,B,[a,b]). % in reverse
    A = [],
    B = [a, b] ;
    A = [a],
    B = [b] ;
    A = [a, b],
    B = [] ;
    false.

    append(A,B,C). % if A B and C all have values, this is an assertion that A B combine to form C


## Let's do some examples with lists

### replace in list - replaces one value with another

### Solution

        replace_in_list(_,_,[],[]).
        replace_in_list(FromItem,ToItem,[FromItem|Tail],[ToItem|ResultTail]) :- replace_in_list(FromItem,ToItem,Tail,ResultTail).
        replace_in_list(FromItem,ToItem,[Item|Tail],[Item|ResultTail]) :-
            FromItem \= Item,
            replace_in_list(FromItem,ToItem,Tail,ResultTail).

### is a member - is a particular value in a list

### Solution

        is_a_member(Item,[Item|_]).
        is_a_member(Item,[_|T]) :- is_a_member(Item,T).

### duplicate members - take a list and duplicate all its elements

### Solution

        duplicate_members([],[]).
        duplicate_members([Head|Tail],[Head,Head|OtherTail]) :- duplicate_members(Tail,OtherTail).

### only repeats - true if a list just contains the same element over and over

### Solution

        all_equal(_,[]).
        all_equal(Item,[Item|T]) :- all_equal(Item,T).
        only_repeats(List) :- all_equal(_,List).

# Prolog 2

A few details:

## Never Not an Unbound Variable

Be really careful with negations in prolog.
They don't always do what you expect.

And never do a not when one of the variables might not be bound.

    isNotSeven(X) :-
        \+(X = Y),
        Y = 7.

## Use is for calculations

    plus2(X,Y) :-
        Y is X + 2.

Note that this does not do the smart thing with unbound variables.

If you use equals with operators you will create compound objects.

    ?- Y = A ^ B, B = C / D.
    Y = A^ (C/D),
    B = C/D

They can be unified, but only if the operators match perfectly.

## Strings

### Not necessarily consistent!  Last year's way (not true anymore)

    ?- X="test",Y='test'.
    X = [116, 101, 115, 116],
    Y = test.
    
    
    ?- string_codes(X,[116, 101, 115, 116]).
    X = "test".

### I reccommend: always single quotes and atom\_chars

    ?- atom_chars('hello', [H|T]).
    H = h,
    T = [e, l, l, o].

Note that not all prolog functions can handle unbound variables.  But
really, what do you expect?

    ?- atom_chars(Y,[h,i]).
    Y = hi.
    
    ?- atom_chars(Y,[h,_]).
    ERROR: atom_chars/2: Arguments are not sufficiently instantiated

## Cuts

### Challenge:

Define a predicate

max(A,B,C) where C is the max of A and B.

### Now lets read about cuts

I think a really good explaination of cuts can be found here:

<http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse43>

I especially like the details of the 2nd example with max.

### Cuts in your homework

    listBind(['?'|T]) :- listBind(T), !.
    listBind([_|T]) :- listBind(T).
    listBind([]).

## Append

A useful function.  Don't forget you can also use it like this:

    append(A,B,[big bound list]).

To try various combinations of a & b.  Can be useful for your homework.

## Work on Word Find HW

# Prolog 3 - a bit on user input

## What Makes Prolog Good?

We've been talking about some of the warts of Prolog and we'll talk
about them some more today.  But I want to talk a bit about why I like
prolog to kick it off.

1. A natural syntax for making programs that approximate human
   reasoning. I think of it as sort of an "SQL for logic".
2. A trade-off between the "pure logic" of theorem provers and a
   straightforward procedural language like vanilla scheme.  One that
   mostly lets you think logically, but also lets you reason about
   performance cost directly.
   
   Note that if tuned, Prolog can actually execute quite quickly
   because it constrains its structure to ones that are efficient to
   check.
3. Unification is just such a elegant way to at the same time
   constrain input and extract the data you need - and indeed will be
   used in some of the other languages we look at in this course.
4. Even on today's computers with lots of resources most of our
   programming is imperative (i.e. it describes a process to get a
   result) not declarative (describes the result we want and lets the
   computer figure out how to give it to us).  But that is not the
   only way to be, language wise.

## Prolog Input Basics

Languages of an AI bent tend to skimp a bit when it comes to input and
output.  Prolog is no exception.  Your basic input function is called
get\_char(X).

    ?- get_char(X), get_char(Y).
    |: hello
    
    X = h,
    Y = e.

Note that this returns in essence a one character atom not a char code

There is also get\_code(X) which will give you the character code if
you wanted that.

## Prolog input challenge

### Write a function that takes in a prolog string (terminated with '\n')

    ?- get_string(X).
    |: hello world.
    X = 'hello world.' .

1.  Solution (my super ugly version)

        get_string(String) :- get_string('','',String).
        get_string('\n',String,String).
        get_string(PrevChar,CurrentString,String) :-
            string_concat(CurrentString, PrevChar, NewCurrent),
            get_char(NextChar),
            get_string(NextChar,NewCurrent,String),
            !.

2.  Another pretty ugly buffalo solution

        get_string(String) :- get_char(C),
                              get_stringH([C|Rest]),
                              append(NoSlashN, ['\n'], [C|Rest]),
                              atom_chars(String, NoSlashN), !.
        get_stringH(['\n']) :- !.
        get_stringH([_,NextChar|Rest]) :-
            get_char(NextChar),
            get_stringH([NextChar|Rest]).

3.  A nice variation by michaea1@ that uses ;

        get_string(X) :- get_string_helper(Y), string_codes(X,Y).
        get_string_helper(X) :- get_code(Y), (Y = 10, X = []; get_string_helper(Z), X = [Y|Z]), !.

4.  A version from hansondg@

    This one is way better than mine.
    
        get_string('\n',[]) :- !.
        get_string(Head,[Head|Result]) :- get_char(Char), get_string(Char,Result).
        
        get_string(Result) :-
            get_char(Char),
            get_string(Char,List),
            atom_chars(Result,List).

### Write a function that takes in a string, and returns a list of strings separated by spaces

    ?- split('hello prolog world',' ', X)
    X = [hello,prolog,world].

Turns out there is a built in predicate for this one -
atomic\_list\_concat.  But I would be curious to see your solutions.

1.  solution from taylorj7@

        isC(X,O) :- X=O,!.
        split(X,R,On) :-
                atom_chars(X,Temp),
                split(Temp,[],[],T,On),rev(T,R).
        split([],[],Work,Work,_).
        split([],X,Work,R,On):-
                rev(X,T),
                string_chars(Temp3,T),
                split([],[],[Temp3|Work],R,On).
        split([C|X],Cons,Working,R,On) :-
                (isC(C,On),
                 rev(Cons,T),
                 string_chars(Temp3,T),
                 split(X,[],[Temp3|Working],R,On),
                 !;
                split(X,[C|Cons],Working,R,On)
                ,!).
2. Solution from brubakbd@

        split(Str, Delim, Ar) :- getString(Word, NewAr, Delim, Str), append([Word], NewAr, Ar).
        getString(Word, Ar, Delim, Str) :- get_first(Char, Str, Rem), helper(Char, Word, Ar, Delim, Rem).
        helper(Char, Word, Ar, Delim, Rem):- 
        Char='\n', Word='', Ar=[], !; 
        Char=Delim, Word='', getString(Y, NewAr, Delim, Rem), append([Y], NewAr, Ar), !;
        getString(Y, Ar, Delim, Rem), string_concat(Char,Y,Word), !.
        get_first(Char, Str, Rem) :- Str='',Char='\n',!; atom_chars(Str, [Char|Temp]), atom_chars(Rem, Temp).

3. Solution from panfilwk@

		atom_split(Atom, Sep, SplitAtoms) :-
		  atom_chars(Atom, List),
		  split(List, Sep, SplitLists),
		  maplist(atom_chars, SplitAtoms, SplitLists).
		 
		split(List, Sep, [SplitHead|SplitRest]) :-
		  append(SplitHead, [Sep|Rest], List) ->
		  split(Rest, Sep, SplitRest);
		  SplitHead = List,
		  SplitRest = []. 

# Prolog 4 - Parsing

## Your Prolog Project

[<HomeworkCode/PrologNLPTwo/readme.md>]

The issues presented today in class are covered in detail here:

[<HomeworkCode/PrologNLPTwo/PrologGrammarRules.pdf>]

## Issue: Parses with variable length

    noun_phrase([the,Noun]) :- is_noun(Noun).
    noun_phrase([Noun]) :- is_noun(Noun).
    is_noun(ninja).
    is_noun(ninjas).
    is_noun(student).
    is_noun(students).
    verb_phrase([attack]).
    verb_phrase([attacks]).

A bit of a problem.

### Solution with some problems

    sentence(X) :-
            append(N,V,X),
            noun_phrase(N),
            verb_phrase(V).

### A more efficient but stranger solution

    sentence(X) :-
            noun_phrase(X,NounRemainder),
            verb_phrase(NounRemainder,[]).
    
    noun_phrase([the,Noun|Rest],Rest) :- is_noun(Noun).
    noun_phrase([Noun|Rest],Rest) :- is_noun(Noun).
    verb_phrase([attack|Rest],[Rest]).
    verb_phrase([attacks|Rest],[Rest]).

### A specialized syntax for the stranger solution

    sentence --> noun_phrase, verb.
    noun_phrase --> determiner, noun.
    noun_phrase --> noun.
    verb_phrase --> verb.
    determiner --> [the].
    noun --> [ninja].
    noun --> [ninjas].
    noun --> [student].
    noun --> [students].
    verb --> [attack].
    verb --> [attacks].

### I recommend you use the basic syntax, but it's up to you

## Issue: Number Agreement

    sentence(X) :-
            append(N,V,X),
            noun_phrase(N),
            verb_phrase(V).
    
    noun_phrase([the,Noun]) :- is_noun(Noun).
    noun_phrase([Noun]) :- is_noun(Noun).
    is_noun(ninja).
    is_noun(ninjas).
    is_noun(student).
    is_noun(students).
    verb_phrase([attack]).
    verb_phrase([attacks]).

The problem is that "the student attack" is a valid sentence.

### Solution

A variable passed between the parsed steps

    sentence(X) :-
            append(N,V,X),
            noun_phrase(Sop,N),
            verb_phrase(Sop,V).
    
    noun_phrase(Sop,[Noun]) :- is_noun(Sop,Noun).
    verb_phrase(Sop,[Verb]) :- is_verb(Sop,Verb).
    
    is_noun(plural, ninjas).
    is_noun(singular,ninja).
    is_noun(plural, students).
    is_noun(singular,student).
    
    is_verb(singular,attacks).
    is_verb(plural,attack).

## Issue: We want to output something

We don't want to know if something parses, we want to output a parse tree.

### Think about it before you peek!

We can use the same trick, we used with signular/plural only with the
parse output.

```
sentence(X,sentence(NT,VT)) :-
        append(N,V,X),
        noun\_phrase(Sop,NT,N),
        verb\_phrase(Sop,VT,V).

noun\_phrase(Sop,noun(Noun),[Noun]) :- is\_noun(Sop,Noun).
verb\_phrase(Sop,verb(Verb),[Verb]) :- is\_verb(Sop,Verb).

is\_noun(plural, ninjas).
is\_noun(singular,ninja).
is\_noun(plural, students).
is\_noun(singular,student).

is\_verb(singular,attacks).
is\_verb(plural,attack).
```

# More Useful Stuff

## Compounds

    compount_name_arguments

## Debugging prolog

<http://www.swi-prolog.org/pldoc/man?section=debugoverview>

Some example code

    parent(frank,tom).
    parent(jane,tom).
    parent(tom,gretchen).
    parent(ben,abbey).
    parent(gretchen,abbey).
    
    is_ancestor(Ancestor,Decendent) :-
            parent(Ancestor,Decendent).
    is_ancestor(Ancestor,Decendent) :-
            parent(Somebody,Decendent),
            is_ancestor(Ancestor,Somebody).

turn trace on to watch how prolog solves it

    1 ?- trace().
    true.
    [trace] 3 ?- is_ancestor(frank,abbey).
       Call: (7) is_ancestor(frank, abbey) ? Options:
    +:                  spy        -:              no spy
    /c|e|r|f|u|a goal:  find       .:              repeat find
    a:                  abort      A:              alternatives
    b:                  break      c (ret, space): creep
    [depth] d:          depth      e:              exit
    f:                  fail       [ndepth] g:     goals (backtrace)
    h (?):              help       i:              ignore
    l:                  leap       L:              listing
    n:                  no debug   p:              print
    r:                  retry      s:              skip
    u:                  up         w:              write
    m:                  exception details
    C:                  toggle show context
       Call: (7) is_ancestor(frank, abbey) ? creep
       Call: (8) parent(frank, abbey) ? creep
       Fail: (8) parent(frank, abbey) ? creep
       Redo: (7) is_ancestor(frank, abbey) ? creep
       Call: (8) parent(_G2050, abbey) ? creep
       Exit: (8) parent(ben, abbey) ? creep
       Call: (8) is_ancestor(frank, ben) ? creep
       Call: (9) parent(frank, ben) ? creep
       Fail: (9) parent(frank, ben) ? creep
       Redo: (8) is_ancestor(frank, ben) ? creep
       Call: (9) parent(_G2050, ben) ? creep
       Fail: (9) parent(_G2050, ben) ? creep
       Fail: (8) is_ancestor(frank, ben) ? creep
       Redo: (8) parent(_G2050, abbey) ? creep
       Exit: (8) parent(gretchen, abbey) ? creep
       Call: (8) is_ancestor(frank, gretchen) ? creep
       Call: (9) parent(frank, gretchen) ? creep
       Fail: (9) parent(frank, gretchen) ? creep
       Redo: (8) is_ancestor(frank, gretchen) ? creep
       Call: (9) parent(_G2050, gretchen) ? creep
       Exit: (9) parent(tom, gretchen) ? creep
       Call: (9) is_ancestor(frank, tom) ? creep
       Call: (10) parent(frank, tom) ? creep
       Exit: (10) parent(frank, tom) ? creep
       Exit: (9) is_ancestor(frank, tom) ? creep
       Exit: (8) is_ancestor(frank, gretchen) ? creep
       Exit: (7) is_ancestor(frank, abbey) ? creep
    true ;
       Redo: (10) parent(frank, tom) ? abort
    % Execution Aborted
    [trace] 4 ?- notrace().
    true.
    
    [debug] 5 ?- nodebug().
    true.
    
    6 ?-

## A Few other details

1.  trace(predicate) will print each time a predicate is evaled
2.  spy(predicate) will break into debug mode when a particular predicate is called
3.  leap (from debug menu) is "continue as normal"
4.  nodebug. turn off everything I think.

# Prolog Metafunctions

The prolog language can be extended in prolog (this is fairly common
among non-compiled languages).  Here's a few things you can do.

## Assert and retract

Assert lets you add stuff to the database.  You can do it from a
query, as here, but more useful is making prolog add stuff to its
database based on user input.

	?- assertz(parent('Bob', 'Jane')).
	?- assertz(female('Jane')).
    % note the double parens on this one
	?- assertz((mother(Child, Mother) :-
	                parent(Child, Mother),
	                female(Mother))).
	

BTW those inputs are just compound atoms of course.

There is also a retract that works the other way.

## Clause and call

    ?- clause(likes(buffalo,X), Z).
    X = ninjas,
    Z = true ;
    X = videogames,
    Z = true ;
    Z = likes(X, videogames).

Evals compound atom like a prolog query - but only 1 clause worth
(e.g. if there's a :- it leaves it to you to continue evaluation if
you want)

	?- call(likes(buffalo,X)).
	X = ninjas ;
	X = videogames ;
	X = buffalo ;
	X = alice ;
	false.

Call evals all the way to completion.  If we were going to do the
final state of the project, I suspect what we would do is make
predicates corresponding to things returned from translation and then
eval them.

## Operators

Operators in prolog are always just sugar for functors (e.g. our usual
predicate things).

    ?- display(1 + 2 + 3).
    +(+(1,2),3)

But if you like you can define your own operators and give them your
own meanings.

    :- op(500, xf, is_cool).
    is_cool(X) :- likes(buffalo, X).

And then you can do stuff like this:

    ?- X is_cool.
    X = ninjas ;
    X = videogames ;
    X = buffalo ;
    X = alice ;
    false.


# Rust 1

## Very Basics

Rust is a low level language and initially doesn't seem terribly weird:

    fn main() {
        
        let command = "my cool command";
        
        println!("handling command \"{}\"\n", command);
        
        
        if command == "exit" {
             std::process::exit(0);
        }
    }


What makes it stand out to me is

1.  Strong but strange guarantees about memory safety
2.  Interesting templating system
3.  General commitment to a featureful modern language without compromising speed

This first lecture and maybe the second is going to be focused on #1

## Variables & Ownership

Most examples come frome here

https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html

### First detail: variables immutable by default


    let x = 5;
    println!("The value of x is: {}", x);
    x = 6; // <- error here
    println!("The value of x is: {}", x);

### Variables are freed automatically

    {
        let s = String::from("hello"); // s is valid from this point forward

        // do stuff with s
        
    }   // this scope is now over, and s is no longer valid
        // hello string is freed

This (incidentally) is different than Java

    
    {
        String s = new String("foobar");
        someOtherObj.name = s; // now 2 things point to the foobar String
        
    }  // s's scope is over, but foobar lives on

Points about this Java code

* If the foobar string was not assigned to someOtherObj.name, in
  theory it could be freed at a similar point.  But in practice it
  probably wouldn't, we would have to wait for the Garbage Collector
  to run
* If a similar thing to otherobject assignment happened in Rust we
  would need to be super careful not to free that string at the end of
  s's scope
  

## Moves

    let s1 = String::from("hello");
    let s2 = s1;

    println!("{}, world!", s1); // ERROR
    
How the error looks


    2 |     let s1 = String::from("hello");
      |         -- move occurs because `s1` has type `String`, which does not implement the `Copy` trait
    3 |     let s2 = s1;
      |              -- value moved here
    4 | 
    5 |     println!("{}, world!", s1);
      |                            ^^ value borrowed here after move


What this is saying:

"Because the type string is not copied on assignment, 'let s2 = s1'
MOVED the ownership of the data to s2.  After this move, s1 is no longer
valid"

This works:

    let s1 = String::from("hello");
    let s2 = s1.clone();

    println!("s1 = {}, s2 = {}", s1, s2);

Note that this is explicitly a copy

This is implicitly a copy

    let x = 5;
    let y = x;

    println!("x = {}, y = {}", x, y);
    
But don't get distracted - the string case is the way almost all
structures in Rust act (specifically, all structures that don't
implement the copy trait).

## Moves and functions

    fn main() {
        let s = String::from("hello");  // s comes into scope
    
        takes_ownership(s);             // s's value moves into the function...
                                        // ... and so is no longer valid here
    
        //DO NOT USE s HERE
        
    } // Here, x goes out of scope, then s. But because s's value was moved, nothing
      // special happens.
    
    fn takes_ownership(some_string: String) { // some_string comes into scope
        println!("{}", some_string);
    } // Here, some_string goes out of scope and `drop` is called. The backing
      // memory is freed.

Passing an object into a function in Rust is giving it up forever

In the above code this is a bit nonsensical, because the
takes\_ownership function is declared in a way that makes it take
ownership of its parameters but it doesn't seem to actually be using
that ownership for anything.  It should probably borrow instead, which
we will talk about soon.

Instead in rust you often intentionally take ownership because you
want to move the data someplace else:

    pub fn new_student(name : String, age : u32 ) -> Student {
         Student  { name, age }
    }


    // when called
    
    let name_string = String::from("Pat");
    let my_student = new_student(name_string,17);
    // name_string is now invalid but I now own my_student
    
## Borrows

    fn main() {
        let s1 = String::from("hello");
    
        let len = calculate_length(&s1);
    
        println!("The length of '{}' is {}.", s1, len);
    }
    
    fn calculate_length(s: &String) -> usize {
        s.len()
    }

calculate\_length "borrows" the string, it doesn't take ownership of
it or copy it.

It's not crazy to think of this as like a pointer - but that indicates
a major potential pitfall, how can we guarantee memory safety when
passing around pointers?  The answer is that rust will ensure safety,
but borrows will be a lot more limited than we expect...

### Limits on Mutable Borrows

So just regular references, your borrows are by default immutable but
you can explicitly declare them mutable.

    fn main() {
        let mut s = String::from("hello");
    
        change(&mut s);
    }
    
    fn change(some_string: &mut String) {
        some_string.push_str(", world");
    }

Mutable borrows have a big restriction - your object can either have
unlimited immutable borrows OR one mutable borrow but not both.

    let mut s = String::from("hello");

    let r1 = &s; // no problem
    let r2 = &s; // no problem
    let r3 = &mut s; // BIG PROBLEM

The corresponding error looks like this:


    error[E0502]: cannot borrow `s` as mutable because it is also borrowed as
    immutable
     --> borrow_thrice.rs:6:19
      |
    4 |     let r1 = &s; // no problem
      |               - immutable borrow occurs here
    5 |     let r2 = &s; // no problem
    6 |     let r3 = &mut s; // BIG PROBLEM
      |                   ^ mutable borrow occurs here
    7 | }
      | - immutable borrow ends here


### A little example

    pub struct Person {
        first : String,
        last : String
    }
    
    
    fn main() {
        let mut me = Person { first : "Mike".to_string() , last : "Hewner".to_string() };
    
        update_name(&mut me, "!");
        
        println!("A person: {} {}", &me.first, &me.last);
    }
    
    fn update_name(person : &mut Person, to_append : &str) {
        person.last.push_str(to_append);
    }

It's easy to append !, hard to append the first name.

# Rust 2 - More Memory & Errors

Discussion in this section are mostly from here https://doc.rust-lang.org/book/ch10-03-lifetime-syntax.html

## Borrows and dangling references

    fn main() {
        let reference_to_nothing = dangle();
    }
    
    fn dangle() -> &String {
        let s = String::from("hello");
    
        &s
    }

We can't let this happen but how we prevent it is gonna make our lives complicated.

This problem can be fixed by return the String directly - thus giving
ownership to the caller.

### Borrows have "lifetimes" associated with them

    {
        let r;

        {
            let x = 5;
            r = &x;
        }

        println!("r: {}", r);
    }

The error this produces is

    7  |             r = &x;
       |                 ^^ borrowed value does not live long enough
    8  |         }
       |         - `x` dropped here while still borrowed
    9  | 
    10 |         println!("r: {}", r);
       |                           - borrow later used here

The key thing to note is all borrows have an implicit lifetime
annotation, very similar to a type annotation.  You can actually even
refer to the implicit lifetime annotation like this:

    &'a i32 

### Lifetimes are complicated once we introduce functions that return references

    fn longest(x: &str, y: &str) -> &str {
        if x.len() > y.len() {
            x
        } else {
            y
        }
    }

This does not compile because if x and y have different lifetimes,
it's not obvious what the lifetime of the result string should be.  It
can be fixed like this:


    fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
        if x.len() > y.len() {
            x
        } else {
            y
        }
    }

This looks like it's requiring the two parameters to have the same
lifetime, but it's a little smarter than that.  It's actually looking
for a lifetime that both x and y can satisfy - meaning the result
lifetime is the shorter of the two parameter lifetimes.

### Returned references also extend the length of borrows

    fn main() {
        let mut me = Person { first : "Mike".to_string() , last : "Hewner".to_string() };
    
        let thing_to_update = get_name_for_update(&mut me);
        thing_to_update.push_str(&me.first); // ERROR!
        
        println!("A person: {} {}", &me.first, &me.last);
    }
    
    fn get_name_for_update(person : &mut Person) -> &mut String {
        &mut person.last
    }

### You probably do not want to use borrows to make linked structures

    fn main() {
    
        let mut students : Vec<String> = Vec::new();
        let mut cool_students : Vec<&String> = Vec::new();
    
        students.push("Alice".to_string());
        students.push("Bill".to_string());
        students.push("Charles".to_string());
    
        cool_students.push(students.get(0).unwrap());
        cool_students.push(students.get(1).unwrap());
    
        //students.push("Dave".to_string());
        //students.pop();
        
        println!("students: {:?}", students);
        println!("number of cool students: {}", cool_students.len())
    }

It's mostly just impossible if things are mutable

### General advice

Use simple containment structures - not networks of interrelationships.

So let's say I have a set of StudentGroup objects, which conceptually
I think of as contain Student objects.  Students can enter and leave
groups, students can be modified.  Students can be added to the system
at any time.

In Java, the StudentGroups would contain student objects, which would
make it easy to iterate across the students in a group and (say) add a
grade to them.

In Rust, I'd probably have the student groups contain StudentIds or
StudentNames as strings or numbers.  Then I'd look up the relevant
student in the student list and update them.


## Errors and safety

### Option Type

So you may notice a lot of rust functions return Option types

    cool_students.push(students.get(0));

This errors

       |
    11 |     cool_students.push(students.get(0));
       |                        ^^^^^^^^^^^^^^^ expected `&String`, found enum `Option`
       |
       = note: expected reference `&String`
                       found enum `Option<&String>`

The option type is similar to the haskell Maybe type.  It represents
something that might have Some particular value, or might have None.
You can break it apart into the various cases if you want:

    let val = students.get(0);

    match val {
        Some(x) => {
            println!("value was {}", x);
        }
        None => {
            println!("abort! students was empty!?!");
        }
    }

The interesting part is the None case.  This is very common where you
do a lookup that could theoretically fail, but if it does fail it
means something catastrophically bizarre has happened.  You don't want
to litter spurious error handling in your code everywhere, so what can
be done?

    let val = students.get(0).unwrap();
    
    // either get the value student(0) contains
    // or panic and abort the whole program

### Propagating errors

Another issue is when you *do* want some sort of error handling and
you want to ensure it happens but yet again you don't want to litter
special cases everywhere.

    stream.write(b"HTTP/1.1 200 OK\r\n");
    stream.write(b"Content-Type: text/html; charset=UTF-8\r\n\r\n");
    stream.write(b"<html><body>Hello world</body></html>\r\n");

One option would be Java-style exceptions, but that actually
introduces a performance penalty.  Instead, rust uses Err types
similar to option types.  It will warn if you don't inspect them,
though you can just unwrap() (that might seem like a bad design, but
it's a good design).

    warning: unused `std::result::Result` that must be used
      --> simple_http_server.rs:19:5
       |
    19 |     stream.write(b"HTTP/1.1 200 OK\r\n");
       |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       |
       = note: `#[warn(unused_must_use)]` on by default
       = note: this `Result` may be an `Err` variant, which should be handled

But usually with these kind of errors - if you do care to handle
them - you're not going to handle each of these lines in a separate
way.  Instead propagate them to someone who cares!

    fn handle_write(mut stream: TcpStream) -> std::io::Result<()>{
    
        stream.write(b"HTTP/1.1 200 OK\r\n") ? ;
        stream.write(b"Content-Type: text/html; charset=UTF-8\r\n\r\n") ? ;
        stream.write(b"<html><body>Hello world</body></html>\r\n") ? ;
    
        Ok(())
    
    }

The ? operator amounts to - if this is an error result, return that
result from the function.  If it is not an error result, unwrap it
(though that unwrapped value is unused in this case).

# Rust 3: Threads

So Rust is a really good language for threading.  It's memory model
naturally supports memory safety, and it has some handy abstractions
for quick and reliable inter-thread communication.  Of course, the
requirement that everything be verifiably safe does limit flexibility
in certain ways, but given how tricky it can be to write correct
concurrent code that tradeoff may well be worth it.

Most of the examples from here

https://doc.rust-lang.org/book/ch16-01-threads.html

## Your basic Rust multi-threaded program

    use std::thread;
    use std::time::Duration;
    
    fn main() {
        let handle = thread::spawn(|| {
            for i in 1..10 {
                println!("hi number {} from the spawned thread!", i);
                thread::sleep(Duration::from_millis(1));
            }
        });
        
        for i in 1..5 {
            println!("hi number {} from the main thread!", i);
            thread::sleep(Duration::from_millis(1));
        }
        
        handle.join().unwrap();

    }

Note Rust's pretty closure syntax.  You'd put a parameter in between
those || if your closure took parameters.

## Passing data through move

Non-functional thread code

    use std::thread;
    use std::time::Duration;
    
    fn main() {
    
        let names = vec!("thread1","thread2", "thread3");
        let mut handles = Vec::new();
        
        for name in names {
            let handle = thread::spawn(|| {
                for i in 1..3 {
                    println!("hi number {} from the thread {}!", i, name);
                    thread::sleep(Duration::from_millis(1));
                }
            });
            handles.push(handle);
        }
    
        for handle in handles {
            handle.join().unwrap();
        }
        
    }

Made safe with move

    use std::thread;
    use std::time::Duration;
    
    fn main() {
    
        let names = vec!("thread1","thread2", "thread3");
        let mut handles = Vec::new();
        
        for name in names {
    
            let cloned_name = name.clone();
            
            let handle = thread::spawn(move || {
                for i in 1..3 {
                    println!("hi number {} from the thread {}!", i, cloned_name);
                    thread::sleep(Duration::from_millis(1));
                }
            });
            handles.push(handle);
        }
    
        for handle in handles {
            handle.join().unwrap();
        }
    
    
    }

## Communicating with Messages

    use std::sync::mpsc;
    use std::thread;
    
    fn main() {
        let (tx, rx) = mpsc::channel();
    
        thread::spawn(move || {
            let val = String::from("hi");
            tx.send(val).unwrap();
        });
    
        let received = rx.recv().unwrap();
        println!("Got: {}", received);
    }



# Rust 4 - Objects & Generics

Most stuff from here https://doc.rust-lang.org/book/ch10-01-syntax.html

## The Pre-basics

"Objects" in rust are structs with added methods

    struct Rectangle {
        width: u32,
        height: u32,
    }
    
    impl Rectangle {
    
        fn new(new_width : u32, new_height : u32) -> Rectangle {
            Rectangle {
                width: new_width,
                height: new_height,
            }
        }
        
    
        fn area(&self) -> u32 {
            self.width * self.height
        }
    }
    
    fn main() {
        let rect1 = Rectangle::new(30,50);
    
        println!(
            "The area of the rectangle is {} square pixels.",
            rect1.area()
        );
    }

This can make things that look and act very much like encapsulated
objects of the OO variety, or it can also be used to make very
ordinary C structs with no (or few) methods.

## Now the interesting stuff - Generic Functions

Let's say we have some duplicate code that looks like this:

    fn largest_i32(list: &[i32]) -> &i32 {
        let mut largest = &list[0];
    
        for item in list {
            if item > largest {
                largest = item;
            }
        }
    
        largest
    }
    
    fn largest_char(list: &[char]) -> &char {
        let mut largest = &list[0];
    
        for item in list {
            if item > largest {
                largest = item;
            }
        }
    
        largest
    }

In an OO language we'd probably solve this with some sort of supertype
or interface (maybe not so easy when when we're talking about
primitives).  In Rust the approach looks similar but has some
differences in practice:

    fn largest<T>(list: &[T]) -> &T {
        let mut largest = &list[0];
    
        for item in list {
            if item > largest {
                largest = item;
            }
        }
    
        largest
    }

This is a generic function (sometimes also called a template).  Note
the parameter type <T>.  A key difference is that it produces the
different functions at compile time - so the compiled code will
actually have 2 versions of largest.

But this doesn't work:

    error[E0369]: binary operation `>` cannot be applied to type `&T`
     --> src/main.rs:5:17
      |
    5 |         if item > largest {
      |            ---- ^ ------- &T
      |            |
      |            &T

We'll talk about traits, which will allow us to solve this problem, in
a bit.

## Generic Structs

    struct Point<T> {
        x: T,
        y: T,
    }
    
    fn main() {
        let integer = Point { x: 5, y: 10 };
        let float = Point { x: 1.0, y: 4.0 };
    }

You can have more than one generic type per struct too:

    struct Point<T, U> {
        x: T,
        y: U,
    }

## Generic Structs with methods

    struct Point<T> {
        x: T,
        y: T,
    }
    
    impl<T> Point<T> {
        fn x(&self) -> &T {
            &self.x
        }
    }
    
    fn main() {
        let p = Point { x: 5, y: 10 };
    
        println!("p.x = {}", p.x());
    }

Note that the impl block's templating does not have be exactly the
same as the type.  For example:

    struct Point<T,U> {
        x: T,
        y: U,
    }
    
    impl<T> Point<T,T> {
        fn x(&self) -> &T {
            &self.x
        }
    }

In this version you can have points with two different types, but only
those with the same time get an implementation of X.

## Traits

Traits in rust are very similar to typeclasses in Haskell

    pub trait Summary {
        fn summarize(&self) -> String;
    }
        
    pub struct Tweet {
        pub username: String,
        pub content: String,
        pub reply: bool,
        pub retweet: bool,
    }
    
    impl Summary for Tweet {
        fn summarize(&self) -> String {
            format!("{}: {}", self.username, self.content)
        }
    }
    
    impl Summary for String {
        fn summarize(&self) -> String {
            self.clone()
        }
    }

Note that you can make existing types implement your traits too, as
I've done with String above.

## Default implementations

    pub trait Summary {
        fn summarize(&self) -> String {
            String::from("(Read more...)")
        }
    }

You can give default implementations to your traits.  Note that this
is pretty much the only thing in Rust that looks like *inheritance*
(where you can pull the implementation of certain methods into your
own struct).  It's not the same though, as you can see, because you
don't inherit fields and you can't substitute subclasses for
superclasses anywhere.

## Traits in functions

Impl will get you what you want frequently

    pub fn notify(item: &impl Summary) {
        println!("Breaking news! {}", item.summarize());
    }

But this is just sugar on the more flexible traint generics bounding
system:

    pub fn notify<T: Summary>(item1: &T, item2: &T) {

You can also require more than one trait

    pub fn notify<T: Summary + Display>(item: &T) {

## Fixing our largest example from the beginning

    fn largest<T: PartialOrd + Copy>(list: &[T]) -> T {
        let mut largest = list[0];
    
        for &item in list {
            if item > largest {
                largest = item;
            }
        }
    
        largest
    }
    
    fn main() {
        let number_list = vec![34, 50, 25, 100, 65];
    
        let result = largest(&number_list);
        println!("The largest number is {}", result);
    
        let char_list = vec!['y', 'm', 'a', 'q'];
    
        let result = largest(&char_list);
        println!("The largest char is {}", result);
    }

# Rust Debugging

So debugging rust is pretty easy if you're familiar with gdb (this is
on unix systems).  If not it can be a little arcane but here's a
little tutorial to get you started:

https://tigercosmos.xyz/post/2020/09/system/debug-gdb/


# Haskell 1

    module Main
      where
    
    main=putStrLn "Hello, World!"

## Pure Functional

    addTwo : Int -> Int
    addTwo num = num + 2

It is a relative of Haskell and therefore is pretty strictly pure
functional.  That is, we want our code to be functions (in the
mathematical sense).  We don't want any "side effects".  Erlang was
functional but it often had side effects - message sends and receives.

## Has Strong Typing

    module Main
        where

    addTwo :: Int -> Int
    addTwo num = num + 2

    main=putStrLn ("Hello, World!" ++ (show (addTwo 3.0))) --breaks


### But also has type inference

    module Main
        where

    addTwo :: Int -> Int
    addTwo num = num + 2

    addTwoImplicit num = num + 2

    main=putStrLn ("Hello, World!" ++ (show (addTwoImplicit 3.0))) --works

    --BREAKS main=putStrLn ("Hello, World!" ++ (show [addTwoImplicit 3.0, addTwo 2]))


### Functions are designed for partial evaluation

    module Main
        where

    add :: Int -> Int -> Int
    add a b = a + b
    
    -- read this carefully
    addTwo :: Int -> Int
    addTwo = add 2

    --alternative form, showing how to turn a binary operator into a partial eval
    anotherAddTwo :: Int -> Int
    anotherAddTwo = (2+)

    main=putStrLn ("Hello, World!" ++ (show (addTwo 3)))

In haskell, functions that take multiple values are actually "curried
functions".  This simply means that they are actually single parameter
functions that return other functions.

A "curried function" expressed in javascript:

    function add (a) {
        return function (b) {
            return a + b;
        }
    }

    add(3)(4);

    var add3 = add(3);

    add3(4);

(Thanks to https://stackoverflow.com/questions/36314/what-is-currying for this example)

In javascript you can make a curried function but usually functions
have multiple parameters.  In Haskell, one parameter functions are all
you really have when you get down to it.

### Activity

Take the code below and change it so it adds the phrase "Buffalo says" before each bit of wisdom:

    module Main
        where

    wisdom :: [String]
    wisdom = ["Ninjas are cool","Do the riskest part first"]

    --use ++ (takes two strings or lists and appends them)
    -- you can partially apply it like this ("foobar" ++)
    --use map (list version, applys a 1 param function to a list of strings)

    main =
        putStrLn $ show wisdom


### Solution

    module Main
        where

    wisdom :: [String]
    wisdom = ["Ninjas are cool","Do the riskest part first"]

    --use ++ (takes two strings or lists and appends them)
    --use map (list version, applys a 1 param function to a list of strings)
    --use partial function evaluation

    main =
        putStrLn $ show $ map ("Buffalo says " ++) wisdom

## Pointfree style

It is common to use haskell's features to express functions as
aggregations/partial evaluations of other functions rather than
explicitly stating their parameters.

Some common tricks:


### $

'$' is like a normal function invocation but it has a really low
priority and is right associative.

    putStrLn $ show wisdom
    
evals show wisdom THEN putStrLn result.  Same as putStrLn (show wisdom)

    putStrLn show wisdom
    
evals "putStrLn show" first which generates a type error


### .

"." in prolog is function composition

    f . g = \x -> f (g x)
    
Note that in this system g is executed first, then the result is passed to f

So if I want a function that acts like the negative version of
absolute value (always makes numbers negative), I can say:

    darkAbs = negate . abs

Note that this can get kinda weird if you functions expect more than
one parameters (because all functions in haskell actually take one
parameter, so they types probably are not gonna work out the way you
expect).

### Omitting parameters

So a parameter applied to the end of a function is just the same as if
the parameter isn't there.

For example this

    sum' xs = foldr (+) 0 xs

and this are the same

    sum = foldr (+) 0



## Hakell's IO

### But more importantly, how do you handle INPUT and State in a pure functional language?

-   Usually the nonfunctional part is provided by a MAGIC FRAMEWORK so
    you only write functional code
-   All the answers tend to revolve around separating pure functional/non-functional
-   Haskell uses the IO Monad (we'll talk about what a Monad is later)

### Very basics

    main :: IO ()
    main = do
        putStrLn "What is your name?"
        name <- getLine
        putStrLn $ "Hey " ++ name ++ " you are cool!"

Seems almost like normal imperative code yes?

But there is some weirdness.  Main is not really a function, at least
by the usual standrd of Haskell (functions have a ->).

Also, what is this *do*

Not obvious, but I hope it's clear that the return type of the do is IO ().

### A function that does IO

    doQuery :: String -> IO String
    doQuery prompt = do
      putStrLn prompt
      result <- getLine
      return $ "response: " ++ result
    
    main :: IO ()
    main = do
      name <- doQuery "What is your name?"
      quest <- doQuery "What is your quest?"
      putStrLn name
      putStrLn quest

The rules of Haskell are that every function when passed the same
parameters return the same result.  Is it true in this case?

Maybe.  It is very hard to tell, because the function doesn't return a
string.  If it did return a string, it would violate the rules of
haskell.  But instead it returns an IO object.  

How do we get into the contents of an IO object?  Using a <- within a
do.  But that makes our own code within an IO object.

# Haskell 2

## Algebraic Data Types

Algebraic I think insofar as you can express them with ors.

These can be used as typechecked enums:

    data QuestionResponse = Yes | No | Maybe | Other String

Or to model data structures (note type parameter):

    data TreeNode a = Leaf a | InnerNode a (TreeNode a) (TreeNode a)

Or even to generate mini class systems:

    data Shape = Rectange Int Int Int Int | Circle Int Int
    
In any case, those capitalized names on the right become constructor
functions that actually return an object of the given type

    someLeaf :: TreeNode String
    someLeaf = Leaf "Hello"

## Record Types

For things with lots of fields, you probably want to use records instead:

    data Person = { firstName :: String, age :: Int, phoneNumer :: String }

Record also have a syntax for returning a copy without enumerating all
the unchanged fields which is nice.

    updateName :: Person -> Person
    updateName p newName = p { firstName = newName }


## Typeclasses

Typeclasses sound a little bit like "classes" but they are a lot
closer to interfaces in Java.

    class Eq a where -- not exactly the real definition
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool

...basically says that a type must implement == and /= to be part of
the Eq typeclass.  You do it like this:

    instance Eq TrafficLight where  
        Red == Red = True
        Green == Green = True  
        Yellow == Yellow = True  
        _ == _ = False
        x /= y = not (x == y)


### Type classes can have default implementations

Here's the real Eq definition

    class Eq a where  
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool
        x == y = not (x /= y)
        x /= y = not (x == y)

The two additional lines are default implementations, which are
provided in terms of the other methods of the typeclass.

These particular defaults make it so you only need to build either ==
or /= and then the other one is implemented for you.  But you can also
set both if you want.

### Type classes can have subclasses

    class (Eq a) => Num a where
        ...

This is just restricting your typeclass to things that already
implement some other typeclass.

### Polymorphism but not inheritance

This provides a great illustration of two terms we teach in 220 but
are often fuzzy for folks.

    printIfEqual :: Eq a => a -> a -> IO ()
    printIfEqual x y = if x == y then putStrLn "equal" else putStrLn "not equal"

The x == y in this case is polymorphic (i.e. it is not calling the
same function if you pass in strings verses if you pass in ints).

In this case, we have code reuse in the sense that *users* of various
classes that are instances of Eq can reuse the same printIfEqual
function for all of them.

BUT, we do not have code reuse in the sense that implementers of a
typeclass reuse how they implement their functions.  

If you want to make a manager which is sort of like a person but also
adds on XYZ, in Haskell you are generally forced to use composition --
inheritance does not exist.

## Typeclasses can get very meta

Sometimes Haskell folks will rag on OO patterns by saying that
patterns in OO languages are just typeclasses in Haskell.  I disagree
with this, for complicated reasons, but I do think it is fair to say
that Haskell folks tend to want to express their patterns as
typeclasses.

## Functors

http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass

Let's do an example and talk about Functors.  So a Functor as a
general category of things is a type that contains a value, but
*usually* acts like the value it contains.

So for example the Maybe Int type usually acts like a regular Int, but
sometimes not.

As is natural in this case, we might want to apply regular functions
to these values (e.g. apply regular Int functions to Maybe Int
objects).  So to be a functor we need a way to take a function that
applies to the regular object and apply it to the Functor object.

Here's maybe:

    instance Functor Maybe where
        -- fmap :: (a -> b) -> Maybe a -> Maybe b (type dec not allowed here)
        fmap f (Just x) = Just (f x)
        fmap f Nothing = Nothing


*Work on running example*

## Applicative Functors

So that is good, but it isn't quite enough because it only works on
single parameter functions.  What if we want to be able to have
multiple parameter functions?

Then we need the applicative functor.  That adds on some qualifications:

    -- this is built into haskell, don't redeclare it in your code
    class (Functor f) => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b


Which in Maybe's instance looks like this:

    instance Applicative Maybe where  
        pure = Just  
        Nothing <*> _ = Nothing  
        (Just f) <*> something = fmap f something  

### My final running example

    import Control.Applicative
    
    data Composite a = Single a | BunchOf [a] deriving (Show)
    
    instance Functor Composite where
    --  fmap :: (a -> b) -> Composite a -> Composite b
      fmap f (Single val) = Single (f val)
      fmap f (BunchOf vals) = BunchOf (map f vals)
    
    instance Applicative Composite where
      pure = Single
      Single f <*> Single val = Single (f val)
      Single f <*> BunchOf vals = BunchOf $ map f vals
      BunchOf fs <*> Single val = BunchOf $ map ($ val) fs
      BunchOf fs <*> BunchOf vals = BunchOf $ foldr (++) [] $ map (\f-> map f vals) fs
    
    main = putStrLn "Hello"


# Elm 4 - Functional Design

## What is the point of design?

-   Make things easier to understand
-   Make things easier to change

### The programs we want to write are complex, how can this be accomplished?

1.  Easy solution

    1.  Divide the program into separate non-duplicated  parts
    2.  Make these parts do one thing that's easy to understand
    3.  Prevent these parts from being too interconnected
    4.  Combine these parts in some easy-to-understand way

2.  Tricky part

    Depending on what you are familiar with certain things might be easier
    to understand or harder.  In that sense a design that seems good from
    some person's perspective might seem arcane from another.  

3.  Guarantees

    There's also a good deal of variation in what we would like to be able
    to guarantee.  Guarantees can be good, but they limit the design
    space.  Think about gotos, for example, or private variables.

4.  Magic solutions

    Sometimes we can also have insights that seemingly complex and varied
    things can be modeled in very uniform straightforward ways.  Sometimes
    this can greatly simplify a design.
    
    In some way, this is the holy grail of designs.
    
    BUT, it's easy to fool yourself and think you have one of these
    radically simple designs.  No reason to stop looking of course - but
    temper your enthusiasm.  
    
    Usually, if you do have something like this, it will be a bit like a
    semaphore.  It will greatly simplify things BUT using it correctly
    will become its own art.

5.  So what does a different language (or framework) do?

    1.  Give you different ways to break things into parts, and combine
        these parts together.  Maybe these parts will more naturally fit
        your problem than some other parts.
    2.  Provide different (or more, or less) guarantees about stuff
    3.  Lie and tell you that this language is the one that will produce
        reliable magical solutions
    
    This is all good stuff.  But remember that much of it is actually in
    your mind, not in the language!

## An Initial Example

An excerpt from the elm mario example&#x2026;this is outdated now but the
principle holds:

    jump {y} m = if y > 0 && m.y == 0 then { m | vy = 5 } else m
    gravity t m = if m.y > 0 then { m | vy = m.vy - t/4 } else m
    physics t m = { m | x = m.x + t*m.vx , y = max 0 (m.y + t*m.vy) }
    walk {x} m = { m | vx = toFloat x
                     , dir = if x < 0 then "left" else
                              if x > 0 then "right" else m.dir }
    
    step (dt, keys) mario =
      physics dt (walk keys (gravity dt (jump keys mario)))

### Some variations

    --could also be written as
    stepV2 (dt, keys) mario =
     mario
     |> jump keys
     |> gravity dt
     |> walk keys
     |> physics dt
    
    --could also be written as 
    -- >> is function composition
    step (dt, keys) =
      jump keys >> gravity dt >> walk keys >> physics dt

## Functional folks love this!

They're not alone either.  This is the same pattern as unix pipes.

    tail -f logFile.txt  | grep ERROR | sed s/ERROR//

### What is good about it?

-   Lots of things can be abstracted this way
-   Can add new steps very incrementally - difficult change existing steps
-   Total separation of concerns - including state
-   Centralizes control - a large part of the magic is in the flow
    -   Which is maybe really hard to understand
    -   But its in one place
-   Natural modeling of processes that consist of discrete steps
-   Each part can be separately unit tested

### What is bad about it?

-   Sometimes things are interdependent and cannot be "layered" correctly
-   Best when data for communicating between steps is simple, worst when
    communication between steps is a massive blob
-   Critical dependence on intermediate data format
-   User input, output, network communication don't fit this model directly

## Let's talk about (idealized) OO paradigm

Objects that each have ownership over some data.  They communicate to
get the job done.

### What is good about it?

-   Lots of things can be abstracted this way
-   Can add new in-object state/features very incrementally - difficult
    to change existing communication patterns
-   Separation of concerns - but you do have state
-   Natural modeling of "things" often makes it possible to guess where
    to find code
-   Distributes control
    -   Oftentimes you have to guess where to look for stuff
    -   But maybe you don't always care

### What is bad about it

-   State becomes very complex quickly - can be very hard to test
-   Best when majority of stuff is happening within objects, worst when
    majority of stuff is in long chains of calls
-   Critical dependence on object's references to each other - this is
    usually setup at runtime
-   Object relationships often hidden throughout the code

## My advice:

Think about how you can think about your processes as transformation
chains.  It's a good model, when it works.  Sometimes it needs some
care to make the chain emerge.

## Polymorphism

Elm is interesting insofar as it does not support any meaningful
polymorphism.  E.g. every function call can be explicitly traced -
there isn't even function overloading.

Instead, you have to use the union types to explicitly enumerate
things.

\#+BEGIN\_SRC elm
update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update message model =
    case message of
        NavigationBarMsg subMsg ->
            let
                ( updatedNavigationBarModel, navigationBarCmd ) =
                    NavigationBar.update subMsg model.navigationBarModel
            in
                ( { model | navigationBarModel = updatedNavigationBarModel }, Cmd.map NavigationBarMsg navigationBarCmd )
        MenuMsg subMsg ->
            let
                ( updatedMenuModel, MenuCmd ) =
                    Menu.update subMsg model.MenuModel
            in
                ( { model | menuModel = updatedMenuModel }, Cmd.map MenuMsg MenuCmd )

\#+END\_SRC elm

HOWEVER, and we'll talk about this in the next class, we can sometime
use functions to do polymorphism-type things and simplify this.

Also, note that lack of polymorphism is not a universal feature of
pure-functional languages.  

# Haskell: Representing Type & State 

## OO Programming and "Types"

The OO paradigm encourages you to think about the world as "types" of
things.

So if we see -say- the various enemies in a video game, the OO
approach is to call them various "types".

This corresponds well to certain intuitions about the world BUT if you
have a lot of types you have to be careful about not having
specialized code for every kind of thing.  This is often immortalized
in our hatred of case statements.

This works well enough (it you're careful) because OO languages have a
robust idea of type similarity and difference (supertypes, interfaces,
shared methods, mixins, etc.).

## In functional approaches we don't want too many types

So we instead see the multiple enemies on the world as "behaving
differently".  And we'll model that behavior with interesting uses of
functions.

For example, this is mostly how I model all items (enemies, bullets,
etc.) in my simple space shooter.

    data Entity = Entity
      {
        pos :: (Float, Float),
        vel :: (Float, Float),
        shade :: Color,
        updateE :: Entity -> [Entity],
        radius  :: Float
      } 

Nothing exciting except this Entity -> Entity function.  This function
allows an Entity to update itself, and it's called by the framework on
every "update" call.

This provides a starting point for making different enemies that
"behave" differently.

## Acting over time 

    wait :: Int -> (Entity -> [Entity]) -> (Entity -> [Entity])
    wait steps f2 = \e ->
        if steps == 0 then
            [e { updateE = f2 }]
        else
            [e { updateE = wait (steps - 1) f2 }]


    dropbomb :: (Entity -> [Entity]) -> (Entity -> [Entity])
    dropbomb thenFunc =
        \e-> let (x,y) = pos e in
            [ e { updateE = thenFunc }, bomb x (y - 20)]



# The final project

Details are [here](FinalProject.docx).


# Instructor's Choice: Smalltalk 1

What to do:
1.  Download Pharo 5 from <http://pharo.org>

    You could try Pharo 6 if you'd like to be more cutting edge, but I
    haven't tried these instructions there.

2.  Start up Pharo and run through the little programmatic tutorial
    included with it.

3.  Then do the tutorial [in sections 1.1 - 1.9 of the PbE book](Homework/SmalltalkPBELightsOut/PharoFirstApplication.pdf)
    
    A hint:
    
    -   Alt shift click appears to meta click, which is how you bring up
        the Morphic "halo"

# Instructor's Choice: Smalltalk, The Image

## How can we be more object-oriented?

-   Make things that aren't objects objects (e.g. ints)
-   Make making big changes easier - editing Object for example
-   Make things that would normally be done with interfaces to
    non-object oriented systems objects instead
-   Make our editing environment use objects

## What's going on with Smalltak

-   When you create a class?
-   When you save?

## What is the image?

It's a binary file representing a system "memory state".

So in a language with an interactive interpreter (e.g. python, prolog, erlang) - imagine you could call a command that would output the state of the system.  Then on a later run you could input that in and all your objects would be recreated, all your variables would be set to their old values, etc.

### Sounds neat, but not really that important

In Smalltalk, 99% of the language is implemented in the image.  Objects only has the methods they have because of the of the state of the Object Class object that lives in the image.  There's no secret separate file that says what Object can do - literally the binary version in the image is the only version of Object there is.

Even the compiler for Smalltalk is implemented as classes in the image.  So you can actually change the way all code compiles by editing objects in the image.

### This seems crazy

This allows Smalltalk to be very "purely" object oriented.  All operations are implemented in terms of objects.  Creating an instance is just telling a Class object to make a new object and give it to you.  Creating a new class is just telling some object to do the right thing.  And you can inspect and modify the way any of this works.

### How do you create an image?

You don't - unless you really want to get involved in heavy sorcery.  Mostly you take an existing image and modify it with very fancy scripts to be what you want.

### How do you share code if it's trapped in an image?

Smalltalk has a method of "fileing out" a particular class or set of classes - basically building a script that will add a class to an existing system or change things from one version to another.  Sometimes you need to augment your fileout with some added code that will add globals you care about or whatever.

## Why is an image good?

Basically it gives you an enhanced programming environment with richer things than files.

### I like files!  You wouldn't believe how good I am at vi.  Plus all my tools like git operate on files.

I am giving this presentation from emacs so you know I sympathize.   But text has some disadvantages:
-   It is not the "true" form of our code, and reconstructing that true
    form is extremely error prone for tools (e.g. autocomplete)
-   There is power in a textual representation (think latex) but
    sometimes other representations are more convenient (think Word)
-   A single file is part of larger code universe, but that universe is
    invisible except during the build process

### An example

Say you wanted to - say - get an email everytime a particular method in a particular class is updated.  How would you do that?

### A programmatic programming environment gives you greater power

1.  You can utilize the same structures the compiler uses

    You know what methods classes have, you can access the abstract syntax trees that the parser makes directly.  You can even augment classes with additional data.  
    
    There is no danger that some critical part of the process (e.g. makefiles, preprocessing) might be doing something strange and this will make your tools work on bad input. (contrast C++ autocompletion)
    
    You could do this before, of course, but you could never be sure that you were doing it right.

2.  You can change the way editing happens!

    To pure text, or text + some visual form, or to whatever you want.  And each form can edit something that's a lot more natural than a raw string.
    
    You could do this before, of course, but you always had to convert to text.  And because text was the first class citizen - it always had to make sense in a textual world first.  For example, you know how you can have a "string constant"?  Why can't you have an "image constant"?

3.  Your tools can easily act on the entire codebase

    Or at least this entire particular product or project.  This makes large scale changes a lot more possible.

## Some examples

### A demo

<http://vimeo.com/97315968> start at 37:33

### The refactoring browser

"We originally thought that the lack of static type-checking would make it hard to build a refactoring browser for Smalltalk. Lack of type information is a disadvantage, but the advantages of Smalltalk made it a lot easier to make a refactoring browser for Smalltalk than it would have have been for C++ or Java."
-   Ralph Johnson

A very approximate chronology:
1992 - First papers describing refactoring published
1997 - Refactoring Browser built, by two graduate students
2001 - First refactoring IDE for Java I can find IntelliJ IDEA
2002? - Eclipse (IBM spent about $40 million on this)



# Instructor's Choice: Haskell and Monads

From the very good and detailed chapters on Monads here:
<http://learnyouahaskell.com/chapters>

## The idea

### Oftentimes we have "almost" pure functions

    coolAdd : Int -> Int -> Int                           
    coolAdd a b = a + b                                   
    {- I sure wish I could log that this was happening! -}

### Of course, we can always add return values!

    coolAddLog : Int -> Int -> (Int,String)                           
    coolAddLog a b = (a + b,"added 2 numbers")

### But the problem is nobody has time for that!

    add3: Int -> Int -> Int -> Int
    add3 a b c = coolAdd c (coolAdd a b)
    
    add3Log: Int -> Int -> Int -> (Int,List String)
    add3Log a b c = 
      let (sum1, log1) = coolAddLog a b
          (sum2, log2) = coolAddLog sum1 c
      in (sum2, [log1,log2])

### Why does this seem annoying?

The issue is that coolAddLog actually conceptually does 2 things:

1.  Return a value like a function
2.  Edit a "context" - in this case the log

### But the context is screwing us up!

1.  Dealing with a context shouldn't greatly uglify our code
2.  It should also be handled in a consistent way that callers can't
    screw up!

### Here's what (we think) we want!

    coolAddLog : Int -> Int -> (Int,String)
    coolAddLog a b = 
        magicallyLog "added 2 numbers";
        a + b

Goodbye sweet pure functional correctness! :(

### How is this different then applicative functors?

So this idea of value + extra "context" is not super different than
what we had with applicative functor.

With those, we again had a value + stuff that we mostly wanted to
treat like a value.  The same is true here - we want our functions
taking ints (that is, boring values) as parameters.

With applicative functors, we "lifted" functions operating entirely on
boring values to functions operating entirely on fancy values.  So in
some sense we had two "tracks" of information - the boring functions,
that operate on ordinary values, and operators that track the "fancy"
aspect.

With monads we have semi-boring functions.  The parameters they take
are normal, but they want to return fancy values.  No problem if they
are standalone, but when we aggregate them we get a problem.  Monads
will give us a way to do that.


## Monads

Note that I play somewhat fast and loose with the syntax here.  The
LoggedValue class I use is purely hypothetical and I switch to Haskell
about 50% of the way down.

### Monadic type = value with some context

    coolAddLog : Int -> Int -> LoggedValue Int
    coolSubtractLog : Int -> Int -> LoggedValue Int
    coolNegateLog : Int -> LoggedValue Int

They'll all **take** ordinary parameters but they'll return functions
with some interesting context.

### Let's combine the functions with some crazy operator!

    coolAddLog 3 4 >>= coolSubtractLog 7 5 >>= coolNegate 8

The >>= could handle evaluating both sides and concating the results
of the logs right?

### Except what if we needed to use the value in a subsequent step?

So the second value actually needs to be a function taking a parameter
of the regular result of the first function.

    coolAddLog 3 4 >>= (\result1 -> coolSubtractLog 3 result1 >>= (\result2 -> coolNegate result2))

BTW, pause for a minute and reflect on those parenthesized functions.
There's nothing strange going on here.

### In Haskell, this can be improved with some syntactic sugar

    foo :: LoggedValue Int
    foo = do
        result1 <- coolAddLog 3 4
        result2 <- coolSubtractLog 3 result1
        coolNegate result2

Just keep in mind what is actually happening here.
1.  Eval coolAddLog 3 4
2.  Eval (result of that >>= a big anonymous function)
3.  As part of that eval, we pass 7 to said big anonymous function
4.  That causes Eval coolSubtract 3 7
5.  Eval (result of 2b >>= a smaller anonymous function)

etc.
1.  Eventually the result returns to the eval on line #2 - it's a LoggedValue
2.  The result of that eval #2 will be a LoggedValue with a value
    equal to the result of the anonymous function call and a log equal
    to the result of the anonymous function call PLUS the log from the
    result of #1
3.  That's what the function as a whole returns

## The elm maybe monad

    import Graphics.Element exposing (..)
    import List exposing (tail, head)
    
    mylist1 = [1, 2, 3]
    mylist2 = [1]
    
    (>>=): Maybe a -> (a -> Maybe b) -> (Maybe b)
    (>>=) maybeVal function =
      case maybeVal of
        Nothing -> Nothing
        Just val -> function val
    
    thirdElement inputList = 
      tail inputList >>= (\result1 -> 
      tail result1 >>= (\result2 -> 
      head result2))
    
    main = show (thirdElement mylist1)

## Write your own monads!

[<HomeworkCode/ElmMonads>]

# Instructor's Choice 2: More on Monads

## Extended example

Where we're going

    view : Model -> Html Msg
    view model =
      toHtml 
      ( 
        emptyPage >>=
        divStart >>=
        add (newButton "-" Decrement ) >>=
        divStart >>=
        add (newLabel (toString model)) >>=
        divEnd >>=
        add (newButton "+" Increment ) >>=
        divEnd
      )

### Step 1

    import Html exposing (Html, beginnerProgram, div, button, text)
    import Html.Events exposing (onClick)
    
    
    type alias Model = Int
    
    newButton buttonText message = 
      button [ onClick message ] [ text buttonText ]
    
    newLabel labelText = text labelText
    
    type alias WebpageMonadicType = List (Html Msg)
    
    main =
      beginnerProgram { model = 0, view = view, update = update }
    
    
    view : Model -> Html Msg
    view model =
      div []
        [ newButton "-" Decrement
        , div [] [ newLabel (toString model) ]
        , newButton "+" Increment
        ]
    
    
    type Msg = Increment | Decrement
    
    
    update msg model =
      case msg of
        Increment ->
          model + 1
    
        Decrement ->
          model - 1

### Step 2: add Monadic Type

    type alias WebpageMonadicType = List (Html Msg)

### Step 3: functions should return monadic type

Let's write a helper to make that easy

    return : Html Msg -> WebpageMonadicType
    return html = [html]

Now our functions become:

\#+BEGIN\_SRC elm
addButton buttonText message = 
  return (button [ onClick message ] [ text buttonText ])

addLabel labelText = 
  return (text labelText)
\\#+END\_SRC elm

### Step 4: let's make our combination operator

    (>>=): WebpageMonadicType -> (Int -> WebpageMonadicType) -> WebpageMonadicType
    (>>=) monadicValue function =
      let otherMonadicValue = function 77 in
        List.concat [monadicValue, otherMonadicValue]

### Step 5: Use it!

    view : Model -> Html Msg
    view model =
      div []
        (addButton "-" Decrement >>= (\ignore ->
        addLabel (toString model) >>= (\ignore ->
        addButton "+" Increment)))

### Step 6: But divs

Lets say we want to have a startdiv and a stopdiv commands.  This
makes things complicated because:

A.  The monadic type is gonna need to be more complex

B.  add fuctions are still not going to care about the monadic type
(or they shouldn't anyway)

C.  startdiv and stopdiv are going to need to modify the monadic type

### Step 7: Update our monadic type

There are a couple ways you could think about doing this, but the one
I went with is this:

    type alias WebpageMonadicType = List (List (Html Msg))

It's a stack of things that will someday become divs.

    divStart: WebpageMonadicType -> WebpageMonadicType
    divStart monadVal = []::monadVal
    
    divEnd: WebpageMonadicType -> WebpageMonadicType
    divEnd monadVal =
      case monadVal of
        h1::h2::tail -> append h2 [div [] h1] :: tail
        _ -> monadVal

### Step 8: Solving the problem of adding

A.  We don't want our HTML producing functions taking a monadic type
(they don't care) 

B.  We especially don't want them returning a monadic type

Our add methods all return Html Msg

What if rather than modify the return type with return, we left the
functions alone, and wrote a function that can covert raw Html Msg
values into a monadic function.

    add: Html Msg -> WebpageMonadicType -> WebpageMonadicType
    add html monadVal =
      case monadVal of
        h::tail -> (append h [html]) :: tail
        _ -> monadVal

### Step 9: Conversion function is easy

    (>>=): WebpageMonadicType -> (WebpageMonadicType -> WebpageMonadicType) -> WebpageMonadicType
    (>>=) monadicValue function =
      function monadicValue

### Step 10: Sugar & we try it out

    emptyPage = [[]]
    
    toHtml : WebpageMonadicType -> Html Msg
    toHtml monadVal =
      case monadVal of
        [[html]] -> html
        _ -> text "Error"
    
    view : Model -> Html Msg
    view model =
      toHtml 
      ( 
        emptyPage >>=
        divStart >>=
        add (newButton "-" Decrement ) >>=
        divStart >>=
        add (newLabel (toString model)) >>=
        divEnd >>=
        add (newButton "+" Increment ) >>=
        divEnd
      )

### Final version

[<HomeworkProblems/ElmMonads/monadWebpage.elm>]

## More examples

### An Example: The Maybe Monad

1.  What is Maybe?

        data Maybe a = Nothing | Just a
    
    It's a type that can either contain a value or be Nothing.  You use it
    when you want to have a value that might be "null".

2.  Why do we want it to be a monad?

    In short it is annoying in a long calculation
    
        add :: Maybe Int -> Maybe Int -> Maybe Int
        add mx my =
          case mx of
            Nothing -> Nothing
            Just x  -> case my of
                         Nothing -> Nothing
                         Just y  -> Just (x + y)
    
    Once something is Nothing, the overall result of the calculation is Nothing.

3.  Surely we can fix it in a complicated way!

    Our functions will be of the form (or similar):
    
        normalFunction :: Int -> Maybe Int
    
    So our concat should act something like this:
    
        applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
        applyMaybe Nothing f  = Nothing  
        applyMaybe (Just x) f = f x

4.  Using Haskell's fancy operator form

        instance Monad Maybe where  
            return x = Just x  
            Nothing >>= f = Nothing  
            Just x >>= f  = f x  
            fail _ = Nothing

5.  What's the deal with return

    The return function is just a way of making a monadic value out of an
    ordinary value.  So in Maybe it just is "Just".  So calculation might
    look like:
    
        sum a b = return (a + b) --equivalent to Just (a + b)
    
    For our hypothetical logger, it would look like this
    
        coolAdd a b = return (a + b) "adding two numbers"

6.  Maybe Monad In Action

        add :: Maybe Int -> Maybe Int -> Maybe Int
        add mx my = do
          x <- mx
          y <- my
          return (x + y)

### Doing IO with Monads

There is something called the IO Monad.

    main = do  
        foo <- putStrLn "Hello, what's your name?"  
        name <- getLine  
        putStrLn ("Hey " ++ name ++ ", you rock!")

1.  The IO Monad prevents your code from being "tainted"

    All IO functions return types appropriate to the IO Monad.
    
    getLine :: IO String
    putStrLn :: String -> IO () 
    
    As a result, they are not compatible with normal values.
    
        nameTag = "Hello, my name is " ++ getLine
    
    (This is an attempt to concat a String and a IO String - not legal)

2.  You can only extract the info within a IO operation

    This means the IO monad "infects" all code that depends on IO.
    
        nameTag :: IO String
        nameTag = do
            name <- getLine
            return ("Hello my name is " ++ name)
    
    This function can never return a type string.  Because it must
    necessarily be the output of a IO typed do.  Or it won't be able to
    actually read the value out of the IO String from getLine.

3.  This encourages just one "main" function that depends on IO

    And pure functions for the rest
    
        nameTagForString :: String -> String
        nameTagForString name = "Hello my name is " ++ name
        
        main :: IO ()
        main = do
            putStrLn "Welcome to Haskell!"
            name <- getLine
            putStrLn (nameTagForString name)

4.  Can getLine really be a pure function?

    Yes!  remember what the do syntax actually means
    
        main = do
            name <- getLine
            putStrLn name
    
        main = getLine >>= (\name -> putStrLn name)
    
    Now what's going on in >>=&#x2026;that's a dark mystery.

### Many other Monads

The cool thing about Monads is not that Haskell found a weird way to
sneak IO into a pure functional language.  It's that you can use this
idea of (pure function + context) to do a lot of interesting stuff.

1.  List

    The standard list is Haskell uses monads to act like nondetermininstic
    computation.
    
        listOfTuples :: [(Int,Char)]  
        listOfTuples = do  
            n <- [1,2]  
            ch <- ['a','b']  
            return (n,ch)  
        
        ---outputs [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

2.  Writer

    Works a lot like the log example I mentioned above.
    
        import Control.Monad.Writer  
        
        logNumber :: Int -> Writer [String] Int  
        logNumber x = Writer (x, ["Got number: " ++ show x])  
        
        multWithLog :: Writer [String] Int  
        multWithLog = do  
            a <- logNumber 3  
            b <- logNumber 5  
            return (a*b)
    
    You can also use it to sum interegers &#x2013; basically anything that
    aggregates values of a certian sort.

3.  State

    Yep - have a modifiable state, using a type of your choice
    
        import Control.Monad.State  
        
        pop :: State Stack Int  
        pop = State $ \(x:xs) -> (x,xs)  
        
        push :: Int -> State Stack ()  
        push a = State $ \xs -> ((),a:xs)  
        
        stackManip :: State Stack Int  
        stackManip = do  
            push 3  
            a <- pop  
            pop

# Buffalo's Guide to a Good Presentation

## Start with the activity in mind

Being a 50 minute presentation, they aren't going to learn much.  So
focusing on the activity gives you a concrete idea...they are going to
do this one thing which hopefully will focus their attention on the
one really cool feature of the language.

## Learning itself is fun

Learning things, even useless things, feels naturally good (most video
games, for example, are just intricately designed teaching systems for
skills no one cares about).  If your students are learning, even if
the presentation is a bit dry, folks will enjoy it anyway.  If folks
are not learning, it's unlikely that your skills as a standup comedian
are sufficient to amuse for 50 minutes.

What this means is that you don't usually need to devote a lot of time
to persuading folks that a language is a good idea.  You also don't
need to make a really complicated example so that it feels realistic
or motivating.

That said, you don't need to make your presentation intentionally
boring.

## Provide Feedback

Make sure your problem is not too hard, but do have unit tests or
something else to ensure that things are going right.

## Explore one topic deeply

Your language probably has many awesome features.  Don't introduce us
to all of them, instead let us get interesting with one really
specific feature.

## Things take time

Expect developing a good example to take some time.  Expect providing
enough of the language to let us do the activity to make some time.
Usually, dumping up a 40 slide deck is the laziest thing you can so
don't expect doing a good job to take less time than that.

Also, practice.

## Check the rubric in the final project document

# Instructor's Choice: Lua and C Integration

Lua is a language that has a lot going for it.

-   No strong typing
-   Very understandable & not too many special constructs
-   First class functions
-   Can use prototype based inheritance - we'll talk about that in
    future classes

## But probably its neatest feature is that is designed to be embedded

in other languages (specifically C).

### An Example

From the nice tutorial here:
<http://www.troubleshooters.com/codecorn/lua/lua_c_calls_lua.htm>

    lua_State *L;
    
    L = luaL_newstate();                        /* Create Lua state variable */
    luaL_openlibs(L);                           /* Load Lua libraries */
    
    if (luaL_loadfile(L, "callfuncscript.lua")) /* Load but don't run the Lua script */
        bail(L, "luaL_loadfile() failed");      /* Error out if file can't be read */
    
    if (lua_pcall(L, 0, 0, 0))                  /* Run lua like a script, defining 
                                                   functions and variables */
        bail(L, "lua_pcall() failed");          /* Error out if Lua file has an error */
    
    lua_getglobal(L, "tellme");                 /* Tell it to run callfuncscript.lua->tellme() */
    if (lua_pcall(L, 0, 0, 0))                  /* Run the function */
        bail(L, "lua_pcall() failed");          /* Error out if Lua file has an error */
    
    lua_close(L);                               /* Clean up, free the Lua state var */

### Communication between lua and C is stack based

    lua_getglobal(L, "square");                 /* Tell it to run callfuncscript.lua->square() */
    lua_pushnumber(L, 6);                       /* Submit 6 as the argument to square() */
    if (lua_pcall(L, 1, 1, 0))                  /* Run function, !!! NRETURN=1 !!! */
        bail(L, "lua_pcall() failed"); 
    
    printf("Back in C again\n");
    int mynumber = lua_tonumber(L, -1);
    printf("Returned number=%d\n", mynumber);

## Why do we want an embedded programming language?

-   To always code extensions in a "safe" environment where we strictly control interaction with our systems
-   To write configuration files that might get really fancy
-   Works great!  Just don't write the language yourself!

## Now you try

### Installing Lua + C integration

For me, on ubuntu16 this worked:

    sudo apt install lua5.3 liblua5.3-dev

Or, if you like you can just ssh into erlang.rose-hulman.edu and use lua there.  But in the case you'll want to checkout your SVN repo on erlang:

    svn co http://svn.csse.rose-hulman.edu/repos/csse403-201720-YOURNETID

### Making sure it works

1.  If you've got lua and C installed on your local system, you can try to just use
    it directly.
2.  checkout the code from your svn repo

3.  First compile luatry2.c

    gcc luatry2.c -I/usr/include/lua5.3 -llua5.3 -o luatry2

1.  Run it like this
    
    ./luatry2

2.  Understand how that code works

3.  Move on to the pcr\_competition activity

### The Paper Scissors Rock Competition Activity

Imagine we want to have a AI programming competition.  Everybody's
going to write an AI that players paper scissors rock, and then they
will compete.

There's a couple ways to do this, but one of the easiest is with an
embedded language.  Contestants will write their code in Lua.  Then
we'll have a C contest runner that loads 2 lua solutions in 2
different lua "universes" and then has them compete.

The only function contestants will need to write is doRound which
takes a parameter of what the opponent played in the last round.  The
contestants can use lua to have any amount of complicated variables,
classes, etc.  In terms of interacting with the external world though,
they will only have the lua functions we specifically enable.

Code is already written for actually running the competition in
pcr\_runner - you just have to add the integration with lua.

# Instructor's Choice 6: Lua and Prototype Based OO

## What is Object Oriented Programming?

My definition (based on Ralph Johnson):
1.  Encapsulation
2.  Inheritance
3.  Polymorphism

Note that this is not necessarily an **endorsement** - I'm just saying
that if your language doesn't have these 3 features the designs you're
going to generate are probably not what I would describe as
object-oriented designs.  They might still be **good** designs though.

### Encapsulation

Object = Data + Methods and this is the preferred organization system.

Note that this is more of a philosophical point than a concrete
language feature.  Some people take this to mean that the language
must support some sort of data hiding (e.g. private variables) that
prevents data from being accessed except through methods.  But there
are plenty of languages I consider OO that don't do this
(e.g. Smalltalk, python).

Further, even if the language supports some sort of data hiding, if
the philosophy is not there I would still not call it OO.  Elm for
example does support data hiding, but its preferred mechanisms
generally suggest manipulating inactive structs.  

### Polymorphism

This tends to be no problem in dynamically typed languages, but is a
significant thing in statically typed languages.  Method call targets
need a way to be determined at runtime (not compile time), and
heterogeneous collections need to be possible.

Some languages allow polymorphism through something akin to
interfaces, rather than using inheritance.

### Inheritance

Objects can derive from other objects, and derived objects have
can be used like their parents BUT they have more stuff.

This tends not to be the case in languages like C - you can have
structs with polymorphic functions, but there is no (easy) way to
extend them.

In dynamically typed languages though, implementing inheritance tends
to add a lot of rules to languages that are otherwise very simple.

## Is there any way to make OO stuff simple for dynamic languages?

Objects in dynamic languages are almost like key-value stores.  But
not quite.

### Very crude solution

Copying objects.

How "classes" would work:
1.  Make an empty hash type object
2.  Modify it by adding fields and methods at keys
3.  Call that object the "prototype"
4.  To make a new instance of the object, copy the prototype
5.  That new object now has all the fields and methods its class had.
    Conceptually it is an instance of the class (but really it's just
    a copy).

How "subclasses" would work

1.  Make a copy of some prototype object
2.  Modify it by some more fields and methods at keys
3.  Class that object the "prototype" of the subclass

1.  Problems

    1.  Shallow copying (good for methods, not good for mutable fields)
    2.  Duplication
    3.  Modifications to superclasses do not affect subclasses (at runtime)

## Solving Duplication/Superclass modification

Allow some sort of key value pair forwarding.  Either:

1.  Some magical parent variable that a map can link to (Javascript &
    IO, self do it this way)
2.  A overridable "key not found" method on your maps.  You can then
    use this to make behavior pretty similar to #1.  Lua uses this.

### How does this work

1.  When you copy an object, don't copy everything (in particular,
    don't copy methods)
2.  Instead, just link the "parent" field of your map to your parent
    prototype

Now method calls just use the parent field to look up the method in
parent, unless you've overridden them

Modifications to parent are propagated to the children

## Solving shallow copy

Oftentimes certain values must be initialized for each copy either
to prevent unwanted field sharing or because these parameters are
idiosyncratic to individual instances.

We want some way to have something a bit like a constructor - could be
an ordinary method that gets called after copy.

Main realization here: you need to be careful about calling your
superclass's clone/initialization behavior either explicitly or
implicitly.

# Instructor's Choice 6: More Prototype Based OO

The topics discussed here are based on the design of Self, 

Getting self successfully running proved difficult so we're going to
have to handle this in the abstract.

This is based on this paper:

http://bibliography.selflanguage.org/_static/organizing-programs.pdf

## Prototypes in Self

1.  No type checking
2.  No classes
3.  Objects are sort of like untyped key-value stores
4.  Instead of being constructed from classes objects are manually
    created by adding and setting "slots", then cloned
5.  Slots can contain methods or data (and indeed you can replace data
    with no-parameter methods transparently)
6.  You can define a special kind of "parent slot" that makes
    unimplemented messages "forward" to that slot.  This is called
    "object inheritance"

## How to implement the basics

### Key insight: it needs to be possible the modify the class later

Can't just use copying straightforwardly because then modifications to
the class after objects are created won't affect existing instances.

### "Class" vs "Instance"

Two parts of a class - the traits and the prototype (you could get by
with just one, but you probably want two)

Parent slot of the prototype is set to trait

You can think of this as the "static" vs "non-static" parts of a
class.

As with classes - you can modify the traits to modify the object
definition and all instances will inherit the effect.

See figure 1a

Sometimes you can get by with only 1 of these:
1.  Abstract classes with no variables
2.  Singleton objects (nil, true, etc.)
3.  Basically function repos (java.Math)

### Inheritance is trait object parents

If you give a trait object a parent, that object becomes your
"superclass".  You don't inherit any instance variables though - but
you can if you want to by making the prototype your "subclasses"
parent as well. (BTW, you can have more than one parent slot)

See 2b and 3

### What if you want multiple representations?

Abstract superclass in class orientations

In prototypes you can just not inherit the representation itself

## Dynamic Inheritance

Let's think about this:

We've got a object/class with some data.  The object can be in one of
3 states.  Depending on that state, many of the methods of the object
ought to change behavior.  In a classic OO language how do you handle
this?

### Prototype Solution

Change your class dynamically - it's a simple as modifying your parent.

Figure 5

### What do you think?

Prototypes: are prototypes a better abstraction that classes?  Discuss.

## Are prototypes a better abstraction than classes?

Your thoughts.

## Using Objects to Store and Categorize

Oftentimes you want to categorize things - like functions.  Consider
java Math for example.  Wouldn't it be nice to maybe group things -
put sine and cosine etc. into a separate part?  But then it'd be
annoying to remember what class they're in!  You'd have to call
AngularFunctions.sine or whatever.

Using object inheritance, we can divide as much as we wish for
conceptual reasons, yet keep the namespaces as universal (or
segregated) as we wish.

Figures 6 and 7

# Final Reflections

## Course Evaluations

-   This is a course I can freely make changes to
-   I am very interested in your feedback
-   Be as honest and detailed as you can

## The Languages

### Prolog

### Erlang

### Elm

### Smalltalk

### Lua

### Your Project Language

## A Reminder of some of the goals in this class

### The Paradigm

Each language represents a paradigm - a unique approach to problem solving.

Using the language gets you into the "head" of the creator
-   as long as you try to do it the "right way"

Even when you're not using the language, the ideas remain

1.  My takeaways

    1.  Prolog
    
        From prolog I hope you learned that languages can have a declarative rather
        than procedural syntax and that this can:
        1.  Make the language look and work in a completely different way
        2.  BUT as a programmer you generally need to understand the
            underlying algorithm - it can't be just "magic" to you
        
        I also hope you learned the idea of unification, which is a
        convenient way of matching on complex structures.
    
    2.  Erlang
    
        From erlang I hope you learned that you achieve a level of parallelism
        and distributedness beyond ordinary applications - and some of the
        pros and the cons of doing so
        
        I hope you realized that a powerful application can be built on simple
        communication mechanisms
        
        And I hope you remember let it fail as an architectural decision for
        interdependent systems
    
    3.  Elm
    
        From elm I hope you learned about programming without state, and how
        that might be possible (and good) but also when it can be annoying
        
        I also hope you got to play around with designs based on a functional
        rather than object based paradigm

### Skills in acquiring new programming languages

1.  In this class you should have learned at least 4 languages well
    enough to accomplish a major project&#x2026;and these languages were
    each very unusual in their own way.
2.  You also learned another 7 enough to do one little assignment

I hope at the end of all of this, you won't be particularly concerned
if you start work at job this summer and they ask you to update a
script that's written in Ruby.

## Remember: paradigm rather than language

There are many alternatives to your basic Java/Python/C# approach to
structuring programs.  Don't allow your vision to be prematurely
narrowed to these few options.

BUT usually the paradigm can exist outside of language itself

E.g.

1.  Nothing's preventing you from limiting the way your Java
    application has side effects.  Yeah, you can't get linguistic
    guarantees, but you can decide as a team to enforce (say) a strict
    stateless MVC paradigm like elm.

2.  If you like the way erlang handles inter-process communication,
    you can build a similar library in most any other language in a
    few days.  Sure, you don't get exactly the benefit of the highly
    optimized BEAM virtual machine but chances are it'd still be
    plenty fast for what you are doing.

3.  Things called "inference engines" integrate functionality much
    like prolog into other languages.  All the features of declarative
    knowledge based reasoning, but you don't have to use it to build
    your GUI!

If you truly understand the paradigm, you don't need the language.
This was what I eventually learned about smalltalk.

### 'Exciting' is a great adjective for a programming language sometimes

A wise software engineer once told me "using new technologies is the
price you pay for getting smart programmers".

Remember that when deploying at scale, technology unknowns can hurt
you badly.

Existing systems are not usually as bad as they might originally
seem - and the REAL problem is not usually lack of strong typing.

### Love the boring languages too!

In this class, I've intentionally shied away from common languages
like C#, Python, etc.  But these languages often contain truly cool
features in their depths, and reward those who study their
intricacies.

## Don't let your exploration end here!

Right now it seems that your are constantly learning tons of new
things.

But a couple of years after graduation, the process will slow

Programming will be part of your job (for most of you) but don't let
the joy of programming disappear

In programming, new stuff is coming all the time both in new frontiers
(new cool, as-yet-undreamed-of languages) and in new parts of
seemingly dusty old languages
