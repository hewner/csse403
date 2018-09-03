
% Part 1: Read section 4.2 in your textbook
%
% Part 2: Write a simple prolog pathfinder

% Here's some example data:
% c stands for 'connected'
:- discontiguous c/2. % because I define test data in 2 parts of the file

c(1,2). % so node 1 and node 2 are connected
c(1,3).
c(2,4).
c(3,4).
c(1,6).

% Write a function findPath that takes a start node, end node and
% computes a path between the start node and the end node.  Note that
% this need not be the *shortest* path, just a path if one exists.
% But if the search continues, of course, it should iterate through
% all paths.

%
% ?- [mazeProblem].
% mazeProblem compiled 0.00 sec, 15 clauses
% true.
%
% ?- findPath(1,4,Path).
% Path = [1, 2, 4] ;
% Path = [1, 3, 4] ;
% false.

% Note that I have included some unit tests for you (they are at the
% bottom of the file).  But these tests are not exhaustive, so be sure
% to test by hand too!
% 
% to run all the tests:
%
% run_tests.
%
% to run just one test:
%
% run_tests(findpath:findpath1).
%
% Part A. 15 points. You can assume no cycles, and edges are directed
%
% To help me write this solution, I wrote a helper function
% findPathHelper that took 3 parameters: the endnode, a reverse
% ordered list of all the nodes in my current path, and the
% final result.  If you use my approach, you'll want to know
% prolog has a built in reverse function for reversing a list.
%
% bonus hint: here's my base case for that solution:
% findPathHelper(Last,[Last|Rest], [Last|Rest]).
%
% You do not need to use my approach though. :D

% a bogus implementation, just to show you the tests working
findPath(Start,End,[Start,End]) :- c(Start,End).

% Part B. 15 points.

% Now your source graph may have cycles. Your solution
% should IGNORE solutions that revisit a node.  So

c(10,11).
c(11,12).
c(12,10).
c(12,13).

c(11,14).
c(14,11).
c(14,15).
c(15,11).
c(15,16).
c(16,13).


% should not get stuck in an infinte loop if I ask for
% a path from 10 to 13.  Nor should it return a path like
% [10,11,12,10,11,12,13]

% Part C. 5 points.  Make it so that the c(X,Y) predicate produces an
% undirected edge.  Note that you shouldn't just add a bunch of
% additional data - instead modify your code so that edges are treated
% like they go both directions regardless of the order they are
% specified in.


% these tests will produce some warnings about "tests succeeded with
% choicepoint", even if your code is written properly.
:- begin_tests(findpath).

test(findpath1) :-
        findPath(1,4,[1,2,4]).

test(findpath2) :-
        findPath(1,4,[1,3,4]).

% some tests with cycles

test(findpath3) :-
        findPath(10,13,[10,11,12,13]).

test(findpath4) :-
        findPath(10,16,[10,11,14,15,16]).

test(findpath5) :-
        findPath(10,13,[10,11,14,15,16,13]).

% ensure you don't return paths with repeats

test(findpath6, [fail]) :-
	findPath(10,13,[10,11,12,10,11,12,13]).

test(findpath7, [fail]) :-
        findPath(10,13,[10,11,14,11,14,15,16,13]).


% some tests with reverse traversal

test(findpath8) :-
        findPath(4,1,[4,2,1]).

test(findpath9) :-
        findPath(4,1,[4,3,1]).

test(findpath10) :-
        findPath(13,10,[13,12,11,10]).

:- end_tests(findpath).
