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

\#+BEGIN\_SRC prolog
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
\\#+END\_SRC prolog

# Debugging prolog

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


# Erlang 1 - Very basics

## Erlang variables & matching

### You can't redefine variables

    28>X = hello.
    hello
    29> X = goodbye.
    exception error: no match of right hand side value goodbye

### You can do prolog-like matching

    39> {Abc,2} = {1,2}.
    {1,2}
    40> Abc.
    1

1.  but it only works one direction

        41>{1,2} = Xyz. 
        41> Xyz.
        2: variable 'Xyz' is unbound

2.  and things can't be in a partially bound state

        42>PartlyBound = {1,2,_}. 
        42> PartlyBound.
        2: variable 'PartlyBound' is unbound

### Atoms, lists, tuples

    atom % these built in "symbols" are very handy for parsing
    {tuple,is,a,specific,length,grouping}
    [list,can,match,with,the,H,Tail,synatx,from,prolog]

Also some pretty neat primitives for mapping bit level stuff
Useful when you want to conserve bandwidth, yet keep stuff expressive

## List functions

List Comprehensions
Many languages have some syntactical sugar for iterating over a list

    for(int x : integers) {
      System.out.print(x + ",");
    }
    
    //As opposed to (approx java from memory here, forgive my mistakes)
    
    Iterator<Integer> i = integers.getIterator();
    while(i.hasNext()) {
       int x = i.next();
       System.out.print(x + ",");
    }

But in languages with more functional feel, you obviously can be a lot cleaner (elisp):

    (mapc (lambda (x) (print x)) '(1 2 3))

In more recent versions of Java and C#, they've gotten on the cool
iteration bandwagon.

### In languages where iteration is not special syntax, you often get a profusion of cool "iterator" functions

RUBY VERSIONS (DO NOT attempt to use on your homework):

    #do something generic to each item
    itemsToPrint.each {|item| puts item } 
    
    #do an operation and make a new list with the results
    doubled = items.collect {|item| item*2 }
    
    # get a subset of the list where something is true
    positives = items.select {|item| item > 0}
    
    # get a subset of the list where something is false
    no_zeros = items.reject {|item| item == 0} 
    
    #check to see if every item in the list has a property
    is_all_evens = items.all? {|item| item % 2 == 0}

In these languages, using these special iterator functions are generally much preferred
to standard loops

## Erlang versions

    % Make anonymous functions like this:
    PlusThree = fun(X) -> X + 3 end.
    
    % Then pass it to iterator function (of course you can do it on one line)
    lists:map(PlusThree, [1,2,3]).
    % produces [4,5,6]
    
    % or completely anonymously
    lists:map(fun(X) -> X + 3 end, [1,2,3]).

foreach - just runs the function and returns the result
map - runs the function and collects the results into a new list
filter - keeps only those that return true
any - returns true if one element returns true
...and more (see your textbook & language docs)

### Write a call using filter removes all empty strings from a list

length("foo") gets the length

### Solution

    lists:filter(fun(X)->length(X) > 0 end,["","","a","","b"]).
    ["a","b"]

## Most complicated foldl (and foldr)

Iterate through the list, keeping a running value
Eg, run through the list and compute the sum

    AddToSum = fun(Item,CurrentSum) -> Item + CurrentSum end.

The new result will become CurrentSum for the next iteration.

The final result is the overall result.

Only other trick is you must pass in an initial value.

    SumList(List) ->
        lists:foldl(AddToSum,0,List).

### Write a function that returns the length of the largest string in a list of strings

0 for an empty list
hint: max(1,2) returns the max of 2 ints

### Solution

    lists:foldl(fun(Item,Max)->max(Max,length(Item)) end,0,["a","bc",""]).

## List Comprehensions

A interesting mix of map,filter,and just a bit of prolog

    % Turn a list of items into a list of {item,item} tuples
    Data = [1,2,3].
    Lists = [ {X,X} || X <- Data ].
    
    % As above, but filter in anything two or higher
    
    Lists = [ {X,X} || X <- Data, X > 1].
    
    % Largest of a pair tuple
    Data = [{1,2},{4,3},{5,6}].
    Lists = [ max(First,Second) || {First,Second} <- Data3].
    
    % Do all possible combinations of a couple values
    % ++ is list/string concat
    
    [ X ++ Y || X <- ["super ","tiny "], Y <- ["ninja","pirate"] ].

### List all values of A B C that make (A or B) and C true

hint: and or and not are boolean operators in erlang
hint: output should be [{true,true,true},{true,false,true},{false,true,true}]

### Solution

    Vals = [true,false].
    [{A,B,C}|| A <- Vals, B <- Vals, C <- Vals, (A or B) and C].

# Erlang 2 - Basic Process

Spawning processes and communicating in erlang is easy!

Update your svn and look at ErlangSimpleCommunication
Take a look at the code in example.erl.
Then try to solve the problem is solveme.erl

My solution is in solvemeSolution.erl but don't peek!

# Erlang 3 - Connecting to a remote erlang server

1.  ssh to remote server
    
        ssh erlang.rose-hulman.edu
    
    Use your EIT password.

2.  start erlang with a long name
    
        erl -name buffalo@erlang.rose-hulman.edu
    
    Note: your name should be UNIQUE - maybe your netid?

3.  start erlang on your local computer using your ip address
    
        erl -name buffalo@137.112.40.209
    
    (Note: type what's my IP into google to find out what it is)
    
    BTW, you'll want to do this in the ErlangSimpleCommunication
    directory so you can load the code.

4.  get the magic cookie from your home computer
    
        (buffalo@137.112.40.209)2> erlang:get_cookie().
        'BLAHBLAHBLAH'

5.  on the remote computer, set its magic cookie to the same thing
    
        erlang:set_cookie(node(),'BLAHBLAHBLAH').
    
    Note: don't use BLAHBLAHBLAH, use whatever your magic cookie actually is

6.  ping your remote computer from your local computer
    
        net_adm:ping('buffalo@erlang.rose-hulman.edu').
        pong
    
    Note: pong is good - pang is bad
    
    In the past, sometimes only one direction will work.  If that
    failed for you, you can try the connection in reverse
    (i.e. connecting from the sever to your local erlang).
    
        (buffalo@erlang.rose-hulman.edu)5> net_adm:ping('buffalo@137.112.40.173').
        pong
    
    Either way, you only have to do one of these.  Once, you do both
    servers will be connected with each other.  You can check by
    running nodes().
    
        (buffalo@137.112.40.173)1> nodes().
        ['buffalo@erlang.rose-hulman.edu']

7.  nl loads your code on all connected servers
    
        (buffalo@137.112.40.173)4> c(solvemeSolution).
        {ok,solvemeSolution}
        (buffalo@137.112.40.173)5> nl(solvemeSolution).
        abcast

8.  You can spawn a process on a remote server like this
    
        RemotePid = spawn('buffalo@erlang.rose-hulman.edu', fun    solvemeSolution:part2_loop/0).
    
    Or
    
        Pid2 = spawn('buffalo@erlang.rose-hulman.edu', fun() -> example:buffalo_counter(0) end).

1.  You can see your process running on the remote server with i() (note this is on the REMOTE server)
    
        (buffalo@erlang.rose-hulman.edu)6> i().
        TONS 'O STUFF followed by
        <0.46.0>              inet_tcp_dist:do_accept/6              610     3983    0
                              dist_util:con_loop/9                    11              
        <0.51.0>              net_kernel:spawn_func/6                233       15    0
                              solveme_sol:part2_loop/0                 1              
        <0.54.0>              net_kernel:spawn_func/6                233       15    0
                              solveme_sol:part2_loop/0                 1              
        Total                                                      47741   399330    0
                                                                     280              
        ok

2.  Send a message to your remote process in the usual way:
    
        (buffalo@137.112.40.226)25> Foo9 ! {test1,2,0.7}.
        {test1,2,0.7}
        Starting test1 Part 2.      
        Finished test1 Part 2. (output: 0.7)
    
    Note that the output of the process is on the local computer, even if it is running on the remote server.
    
    If you want to see output on the executing server, use erlang:display.  For example:
    
        spawn('buffalo@erlang.rose-hulman.edu', fun() -> erlang:display("hello") end).

3.  If you have time, try to write a new function in the SimpleCommunication project that starts up both the spawned part1 processes and the part 2 loop on two different servers.

# Erlang 4 - Let it crash

See the [example code](Homework/ErlangLetItCrashExample).

# The philosophy

What is the correct ratio of try to catches?

# linked in death

So in erlang, it can be pretty common to spawn a subprocess that is
integral to your own process. This should make you concerned, insofar
as it means part of your system can fail and another part lives on,
oblivious to the fact that it's waiting for a message that can never
arrive.

Solution?  A death pact:

    link(Pid)

This causes your process to die if Pid dies.

# More advanced features

    process_flag(trap_exit, true)

This lets you catch the secret message EXIT, which is the thing that
would normally kill your process if you are linked.

# Erlang 5 - Final Assignment, Raft Algorithm

## What is an consensus algorithm?

1.  Algorithm where state is distributed across multiple members.
2.  The problem is consistency - you want to be able to store data when
    not all members are available, BUT you don't want it to be possible
    to get into inconsistent state.  This can be a problem when network
    partitions occur and cause members to leave/rejoin the pool. (let's
    do an example)
3.  We rely on the idea of a majority.  If we require a majority of
    members to agree to something to consider it committed, this
    ensures that any subsequent majority must share at least one member
    in common with a previous majority.
4.  That said, the protocol tends to be complex, because no message can
    be trusted to arrive.

## The Raft algorithm

<http://thesecretlivesofdata.com/raft/>

1.  Raft relies on the idea of a "leader" who serves for a term.
2.  The leader receives requests for updates, sends updates to all
    members, gets responses, then when a majority of members respond,
    considers the update "committed".
3.  Because of the way the raft algorithm elections work, something
    that is committed will definitely be in the log of any electable
    leader.
4.  A leader may encounter a follower that is not up to date.  Such a
    follower will not accept new data.  The leader transmits larger
    and larger logs, going further into the past, until it encounters
    a point of commonality with its follower.  Once a point of
    commonality is found, the follower replaces any data they have not
    in common with the leader's version.

## Your assignment

Only the data transmission part of the Raft algorithm.  We won't do
elections.

[<HomeworkCode/ErlangRaft/raft.erl>]

# Erlang 6 - Debugging sends and receives

## The basics

This command can let you debug a process you are starting:

    7> dbg:c(mergesort,basic1_test,[],[s,r]).
    (<0.193.0>) <0.194.0> ! {sort,[2,5,7],<0.193.0>}
    (<0.193.0>) <0.195.0> ! {sort,[34,2,1],<0.193.0>}
    (<0.193.0>) <0.196.0> ! {sort,[99,11,2],<0.193.0>}
    (<0.193.0>) << {sorted,[2,5,7],<0.194.0>}
    (<0.193.0>) << {sorted,[1,2,34],<0.195.0>}
    (<0.193.0>) << {sorted,[2,11,99],<0.196.0>}
    (<0.193.0>) <0.195.0> ! {merge,[1,2,34],[2,5,7],<0.193.0>}
    (<0.193.0>) << {merged,[1,2,2,5,7,34],<0.195.0>}
    (<0.193.0>) <0.195.0> ! {merge,[1,2,2,5,7,34],[2,11,99],<0.193.0>}

BUT it's not really what you want if your goal is to debug a Raft unit
test.

## Debugging a raft unit test

### Install the trace in the test setup function

Since the raft processes are short lived in the unit tests, we need to
add the instrumentation in the test setup.

    setup() ->
        start_raft_member(raft1),
        start_raft_members([m1,m2,m3]),
        Result = dbg:p(whereis(raft1),[s,r]),
        io:format("Debugging ~p", [Result]).


### Enable the trace

    6> dbg:tracer().                     
    {ok,<0.57.0>}

### Run the test case

    7> eunit:test(raft:ae_hist4_test_()).
    (<0.97.0>) << {<0.102.0>,{append_entries,1,0,0,[{1,newdata}],0}}
    (<0.97.0>) <0.102.0> ! {x,{1,true},[raft1,1]}
    (<0.97.0>) << {<0.102.0>,{append_entries,1,1,1,[{1,bad1}],0}}
    (<0.97.0>) <0.102.0> ! {x,{1,true},[raft1,2]}
    (<0.97.0>) << {<0.102.0>,{append_entries,2,2,1,[{2,bad2}],0}}
    (<0.97.0>) <0.102.0> ! {x,{2,true},[raft1,3]}
    (<0.97.0>) << {<0.102.0>,{append_entries,3,3,3,[{3,newdata4}],0}}
    (<0.97.0>) <0.102.0> ! {x,{3,false},[raft1]}
    (<0.97.0>) << {<0.102.0>,{append_entries,3,2,3,[{3,newdata3},{3,newdata4}],0}}
    (<0.97.0>) <0.102.0> ! {x,{3,false},[raft1]}
    (<0.97.0>) << {<0.102.0>,
                   {append_entries,3,1,1,
                                   [{3,newdata2},{3,newdata3},{3,newdata4}],
                                   0}}
    (<0.97.0>) <0.102.0> ! {x,{3,true},[raft1,4]}
    (<0.97.0>) << {<0.102.0>,{get_term}}
    (<0.97.0>) <0.102.0> ! 3
    (<0.97.0>) << {<0.102.0>,{get_commit_index}}
    (<0.97.0>) <0.102.0> ! 0
    (<0.97.0>) << {<0.102.0>,{get_log}}
    (<0.97.0>) <0.102.0> ! [{1,newdata},{3,newdata2},{3,newdata3},{3,newdata4}]
      Test passed.
    ok

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


### An aside: Functions are designed for partial evaluation

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

### Activity

Take the code below and change it so it adds the phrase "Buffalo says" before each bit of wisdom:

    module Main
        where

    wisdom :: [String]
    wisdom = ["Ninjas are cool","Do the riskest part first"]

    --use ++ (takes two strings or lists and appends them)
    --use map (list version, applys a 1 param function to a list of strings)
    --use partial function evaluation

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
    
## Hakell's IO

### But more importantly, how do you handle INPUT and State in a pure functional language?

-   Usually the nonfunctional part is provided by a MAGIC FRAMEWORK so
    you only write functional code
-   All the answers tend to revolve around separating pure functional/non-functional
-   Helm uses the IO Monad (we'll talk about what a Monad is later)

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



# Elm 2 - Subscriptions & Other complexity

## Subscriptions

This is a reference from the general elm tutorial:

<https://www.elm-tutorial.org/en/03-subs-cmds/01-subs.html>

### What are subscriptions?

Subscriptions are Elm's way of handling events not generated from view
objects (things like buttons or HTML text boxes don't need
subscriptions).  But things like timers, mouse movement, etc. are
handled using the subscription system.

### How do you subscribe?

There is a subscription function that takes the Model and returns Sub
Msg (i.e. a subscription that returns the universal message type).

    subscriptions: Model -> Sub Msg
    subscriptions model =
      Mouse.moves (\{x, y} -> Position x y)

This means you can subscribe to different stuff over the course of a
programs lifetime.

You can also subscribe to more than one thing:

    type Msg
        = MouseMsg Mouse.Position
        | KeyMsg Keyboard.KeyCode
    
    subscriptions : Model -> Sub Msg
    subscriptions model =
        Sub.batch
            [ Mouse.clicks MouseMsg
            , Keyboard.downs KeyMsg
            ]

&#x2026;though as always ALL events must return a universal message type.

### You must also register your subscription function

&#x2026;in a slightly more advanced version of main.

    main =
      Html.program
        { init = ({x = 0,y = 0}, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init = blahblah is just setting the starting state of the model.
Ignore the Cmd for now - we'll talk about that later.

### What happens then?

Your update function will start getting events for your subscriptions,
as well as the previous stuff from the view.

### An Example

    import Html exposing (Html, text, div)
    import Mouse exposing (..)
    
    main =
      Html.program
        { init = ({x = 0,y = 0}, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
    
    -- MODEL
    
    type alias Model = {
      x: Int
      , y : Int
    }
    
    
    -- UPDATE
    
    type Msg
      = Position Int Int
    
    update: Msg -> Model -> (Model, Cmd a)
    update msg model =
      case msg of
        Position x y ->
          ({model | x = x, y = y} , Cmd.none)
    
    -- SUBSCRIPTIONS
    
    subscriptions: Model -> Sub Msg
    subscriptions model =
      Mouse.moves (\{x, y} -> Position x y)
    
    -- VIEW
    
    view: Model -> Html a
    view model =
      text (toString model)

### An Activity

Starting from an example code above, modify this function so as well
as counting the mouse's position it tracks the number of clicks.

To help, look here for documentation
<http://package.elm-lang.org/packages/elm-lang/mouse/1.0.1/Mouse>

plus the example code above where I talk about how to subscribe to
more than one thing

### My Solution

    import Html exposing (Html, text, div)
    import Mouse exposing (..)
    
    main =
      Html.program
        { init = ({x = 0,y = 0, clicks = 0}, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
    
    -- MODEL
    
    type alias Model = {
        x : Int
      , y : Int
      , clicks : Int
    }
    
    
    -- UPDATE
    
    type Msg
      = Move Int Int | Click
    
    update: Msg -> Model -> (Model, Cmd a)
    update msg model =
      case msg of
        Move x y ->
          ({model | x = x, y = y} , Cmd.none)
        Click -> ( {model | clicks = model.clicks + 1}, Cmd.none)
    
    -- SUBSCRIPTIONS
    
    subscriptions: Model -> Sub Msg
    subscriptions model =
      Sub.batch
       [ Mouse.moves (\{x, y} -> Move x y),
         Mouse.clicks (\{x, y} -> Click)]
    
    -- VIEW
    
    view: Model -> Html a
    view model =
      text (toString model)

## Commands

See 

<https://www.elm-tutorial.org/en/03-subs-cmds/02-commands.html>

Commands allow you to issue commands to the runtime to do non-pure
things on your behalf.  This is how randomness can be implemented, for
example.

### How you set it up

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            Roll ->
                ( model, Random.generate OnResult (Random.int 1 6) )
    
            OnResult res ->
                ( res, Cmd.none )

In this case, on the roll update message, issue a command that
generates a random int.

**NOTE** that the function Random.int does not generate a random int.  If it did, We could say this:

    (OnResult (Random.int 1 6)) -- NOT LEGAL

Instead the function Random.generate takes a function of type Int ->
Msg that it will apply to the random number once it's generated by the
runtime.

### A complete example

    module Main exposing (..)
    
    import Html exposing (Html, div, button, text, program)
    import Html.Events exposing (onClick)
    import Random
    
    
    -- MODEL
    
    
    type alias Model =
        Int
    
    
    init : ( Model, Cmd Msg )
    init =
        ( 1, Cmd.none )
    
    
    
    -- MESSAGES
    
    
    type Msg
        = Roll
        | OnResult Int
    
    
    
    -- VIEW
    
    
    view : Model -> Html Msg
    view model =
        div []
            [ button [ onClick Roll ] [ text "Roll" ]
            , text (toString model)
            ]
    
    
    
    -- UPDATE
    
    
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            Roll ->
                ( model, Random.generate OnResult (Random.int 1 6) )
    
            OnResult res ->
                ( res, Cmd.none )
    
    
    
    -- MAIN
    
    
    main : Program Never Model Msg
    main =
        program
            { init = init
            , view = view
            , update = update
            , subscriptions = (always Sub.none)
            }

### Activity

The above code simulated a six sided dice.  Imagine we want to play a
game that uses a six sided dice and a 100 sided dice.  Add a second
button that rolls the 100 sided dice.  

The system should display both results on a single page and rolling
the 100 sided dice should not affect the six sided dice and vice
versa.

### My Solution

    import Html exposing (Html, div, button, text, program)
    import Html.Events exposing (onClick)
    import Random
    
    
    -- MODEL
    
    
    type alias Model =
        {d6 : Int, d100 : Int}
    
    
    init : ( Model, Cmd Msg )
    init =
        ( {d6 = 0, d100 = 0}, Cmd.none )
    
    
    
    -- MESSAGES
    
    
    type Msg
        = Roll
        | Roll100
        | OnResult Int
        | OnResult100 Int
    
    
    
    -- VIEW
    
    
    view : Model -> Html Msg
    view model =
        div []
            [ button [ onClick Roll ] [ text "Roll 6" ],
              button [ onClick Roll100 ] [ text "Roll 100" ]
            , div[] [text (toString model)]
            ]
    
    
    
    
    -- UPDATE
    
    
    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            Roll ->
                ( model, Random.generate OnResult (Random.int 1 6) )
            Roll100 ->
                ( model, Random.generate OnResult100 (Random.int 1 100) )
            OnResult res ->
                ( {model|d6 = res}, Cmd.none )
    
            OnResult100 res ->
                ( {model|d100 = res}, Cmd.none )
    
    
    
    -- MAIN
    
    
    main : Program Never Model Msg
    main =
        program
            { init = init
            , view = view
            , update = update
            , subscriptions = (always Sub.none)
            }

# Elm 3 - Datatypes, Graphics, Composing

## Datatypes

### Elm has a record type!

1.  Creating

        point2D = { x=0, y=0 }
        
        point3D = { x=3, y=4, z=12 }
        
        bill  = { name="Gates", age=57 }
        steve = { name="Jobs" , age=56 }
        larry = { name="Page" , age=39 }

2.  You can use it to make "generic" functions

        dist {x,y} = sqrt (x^2 + y^2)
    
    Or give a particular collection of fields/types an alias:
    
        type alias Point =
          { x : Float
          , y : Float
          }
        
        hypotenuse : Point -> Float
        hypotenuse {x,y} =
          sqrt (x^2 + y^2)
    
    But these are not "true" types in that they can not be self referential.

3.  You can update just one field, which makes updating state easier

        { point2D | y = 1 }
        { point3D | x = 0, y = 0 }
        { steve | name = "Wozniak" }
    
    Note that I've occasionally discovered problems with strict typing and
    this, but type inference always seems to work correctly.  This could well be fixed in the new version of elm though.

4.  You can also use them to store functions and homebrew your own polymorphic objects

        manhattanPoint = { x=3, y=4, distance a b = abs(a.x - b.x) + abs(a.y - b.y) }
    
    Storing functions can definitely be useful.  I'm a lot less sure about
    the polymorphic objects.

### Elm also has "Union Types"

    type Visibility = All  | Active | Completed

Basically a 'global' enum, but unlike Erlang you do get strong typing.

1.  But these can contain data, and you can use case statements to map them

        type Widget
            = ScatterPlot (List (Int, Int))
            | LogData (List String)
            | TimePlot (List (Time, Int))
        
        view : Widget -> Element
        view widget =
            case widget of
              ScatterPlot points ->
                  viewScatterPlot points
        
              LogData logs ->
                  flow down (map viewLog logs)
        
              TimePlot occurrences ->
                  viewTimePlot occurrences
    
    Note the use of tuples.  That's not strictly necessary, but it is a good idea because it allows an approximate "encapsulation" of the data.

### Maybe - cool or just a way to have null?

    type Maybe a = Just a | Nothing

First note that this is a parameterized type.

It can be used like this

    if n > 0 && n <= 12 then Just n else Nothing

### Maybe makes pure functional programmers really happy!

"This may seem like a subtle improvement, but imagine all the code you
have where you defensively added a null check just in case someone
else behaves badly. Having contracts means you have a guarantee that a
caller won't send you bad data! This is a world where you never again
have to spend 4 hours debugging a null pointer exception!"

Maybe like the difference between checked an unchecked exceptions?

### Union Types Can Be Recursive

    type Tree a = Empty | Node a (Tree a) (Tree a)

## Drawing

The new version of elm does drawing using SVG.

\#+BEGIN\_SRC elm
view : Model -> Html Msg
view model =
   svg [ viewBox "0 0 500 500", width "500px" ]
     [ rect [ x (toString model.x), y (toString model.y), width "60", height "10", fill "#0B79CE" ] []
     ]
\\#+END\_SRC elm

The parameters are the parameters of the raw SVG tags in the spec, so you'll want to look them up.

<https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Basic_Shapes>

## Elm & Composing Widgets

<https://www.elm-tutorial.org/en/02-elm-arch/06-composing.html>

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

# Elm 5 - Being Tricky With Functions

## Course Logistics: Midterm grades

-   Grades are submitted
-   If you did not get at least 50% on the Erlang project please talk to
    me

## Course Logistics: ElmVideoGame Posted

## BulletExample

Take a look at the BulletExample.  

[<HomeworkCode/ElmBulletExample/bulletExample.elm>]

See if you can understand how the
code works.  How is the bullet state being stored?

## Using Functions to Store State

In nonfunctional languages there's a tendency to want to store stuff
like enums or type data that must then be reused.

    if(bulletType == straightBullet) {
       // move the bullet in a straightWay
    }
    if(bulletType == sineBullet) {
       // update the bullet like a sine wave
    }

Or maybe you make different types of objects:

    bullet.doUpdate(); //polymorphically do the right thing

But in a functional language - why store something as an object when
you can store it as a partially evaluated function?  Read this code
carefully!

    type BUpdater = BUpdater (Float,Float) (Float -> BUpdater) 
    
    straightBulletUpdate : Float -> Float -> Float -> BUpdater
    straightBulletUpdate x y delta =
        let newX = x + delta/50
            newY = y
        in BUpdater (newX, newY) (straightBulletUpdate newX newY)

We can even use the function to encode arbitrary mutable state:

    sineBulletCreate x y =
        sineBulletUpdate (x+10) y (x+10) y 0
    
    sineBulletUpdate x y initialX initialY delta =
        let newX = x + delta*3
            deltaX = newX - initialX
            newY = initialY + 50*sin (deltaX/15)
        in BUpdater (newX, newY) (sineBulletUpdate newX newY initialX initialY)

## Using functions to be a state machine

Oftentimes we want to model state machines with different states (and
frequently a big complex case statement).  But if our states are
functions, doing the right thing is just the same as executing the
functions.  And changing state is just changing the function we're
partially evaluating.

    mineBulletUpdate x targetX y delta = 
        let newX = x - delta*4
            newY = y
        in if(x > targetX) then
               BUpdater (newX, newY) (mineBulletUpdate newX targetX newY)
           else
               doNothingUpdate newX newY delta
    
    doNothingUpdate x y delta = BUpdater (x,y) (doNothingUpdate x y)

You'll have to change the step function to see this code in action.

# The final project

Details are on Moodle.  Proposals are due Wednesday!

# Instructor's Choice 1: Haskell and Monads

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

etc&#x2026;
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

# Work day

## A bit on randomness in elm

    import Html exposing (text)
    import Random exposing (..)
    
    main =
      let generator = float 0 1
          seed1 = initialSeed 3
          (val1, seed2) = step generator seed1
          (val2, seed3) = step generator seed2
          (val3, seed4) = step generator seed3
      in
      text (toString [val1, val2, val3])

The annoyance of having to pass the seed around is what we're
trying to avoid here.

## Make sure you get your project completely signed off on

## Elm Monads assignment

# Instructor's Choice 3: Smalltalk 1

What to do:
1.  Download Pharo 5 from <http://pharo.org>

2.  Start up Pharo and run through the little programmatic tutorial
    included with it.

3.  Then do the tutorial in sections 1.1 - 1.9 of the PbE book.  
    
    [<HomeworkCode/SmalltalkPBELightsOut/PharoFirstApplication.pdf>]
    
    A hint:
    
    -   Alt shift click appears to meta click, which is how you bring up
        the Morphic "halo"

# Instructor's Choice 4: Smalltalk, The Image

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

# Instructor's Choice 5: Lua and C Integration

Lua is a language that has a lot going for it.

-   No strong typing
-   Very understandable & not too many special constructs
-   First class functions
-   Can use prototype based inheritance - we'll talk about that in
    future classes

## But probably its neatest feature is that is designed to be embedded

&#x2026;in other languages (specifically C).

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
object&#x2013;oriented designs.  They might still be **good** designs though.

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

Oftentimes certain values must be initialized for each copy&#x2026;either
to prevent unwanted field sharing or because these parameters are
idiosyncratic to individual instances.

We want some way to have something a bit like a constructor&#x2026;could be
an ordinary method that gets called after copy.

Main realization here: you need to be careful about calling your
superclass's clone/initialization behavior either explicitly or
implicitly.

# Instructor's Choice 6: More Prototype Based OO

The topics discussed here are based on the design of Self, 

Getting self successfully running proved difficult so we're going to
have to handle this in the abstract.

This is based on this paper:

<http://bibliography.selflanguage.org/_static/organizing-programs.pdf>

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
"superclass".  You don't inherit any instance variables though &#x2013; but
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
