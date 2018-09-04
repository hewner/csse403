likes(buffalo, ninjas).
likes(buffalo, videogames).
% do an example with likes prolog filling in stuff

% get an example from the class
% find somebody who also likes video games
likes(steveo, running).
likes(alice, videogames).

% then make a implication
likes(buffalo,X) :- likes(X,videogames).
% comma is like "and"
likes(alice,X) :- likes(X,videogames), likes(X,ninjas).

% how does this work?  Unification.

example(ninja,pirate,robot).
example(zombie,dino,alien).
example(X,Y,Z) :- example(Y,X,Z). % note that this causes an infinite loop

isNotSeven(X) :-
    \+(X = Y),
    Y = 7.

isNotSeven2(X) :-
    X \= 7.

    
max(A,B,B) :- B > A, !.
max(A,B,A).

get_string('\n',[]) :- !.
get_string(Head,[Head|Result]) :- get_char(Char), get_string(Char,Result).

get_string(Result) :-
    get_char(Char),
    get_string(Char,List),
    atom_chars(Result,List).

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

replace_in_list(_,_,[],[]).
replace_in_list(FromItem,ToItem,[FromItem|Tail],[ToItem|ResultTail]) :- replace_in_list(FromItem,ToItem,Tail,ResultTail).
replace_in_list(FromItem,ToItem,[Item|Tail],[Item|ResultTail]) :-
    FromItem \= Item,
    replace_in_list(FromItem,ToItem,Tail,ResultTail).

foo(3,Y,[Y,Z]). 
