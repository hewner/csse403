-module(list_probs).
-export([add_val/2,print_with_parens/1,reverse_all/1,member/2,subtract/2,append_strings/2,append_strings2/2, evens/1,sum_to_10/1,swap_tup/1,get_values/2]).

% Read section 6.3 of your textbook
% Especially "Applying functions to lists" and "List Comprehensions"
%
% All problems in this set are with 3 points, for a total of 30 points

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART A: LIST FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For these problems, don't use recursion.  Instead make use of
% list functions supplied by lists for the first part.  
%
% For Example:
%
% A function that takes a list of integers and adds a given
% value to them.
%
% 2> list_probs:add_val([0,1,2,10],7).
% [7,8,9,17]

add_val(List,Val) ->
    lists:map(fun(X) -> X + Val end, List).

% A function that prints (not returns) a bunch of numbers with parens around them
% No need to worry about writing a final newline
%
% 9> list_probs:print_with_parens([5,44,2]).
% (5)(44)(2)ok

print_with_parens(List) ->
    solveme.

% A function that takes a list of lists and reverses them
% hint: there is a function lists:reverse you might find useful
%
% 15> list_probs:reverse_all([[1,2],[3],[4,5,6]]).
% [[2,1],[3],[6,5,4]]

reverse_all(ListOfLists) ->
    solveme.

% returns true if an item is in a list
%
% there is already a built in function lists:member that does
% this, but just for practice.
%
% 22> list_probs:member(3,[2,3,4]).
% true
% 23> list_probs:member(3,[5,6,7]).
% false

member(Item,List) ->
    solveme.
                       
% subtracts 2 lists (i.e. removes all the elements of the 2nd list from the first)
% hint: don't forget about member from the last problem
%
% 30> list_probs:subtract([1,2,3,4,1,5,2],[1,2]).
% [3,4,5]
subtract(List1,List2) ->
    solveme.

% appends a list of strings to the end of an existing string
% hint: use foldl
%
% this can be done without using the apply functions list operations
% bit it is still a reasonable example
%
% 38> list_probs:append_strings("hello",["world","erlang"]).
% "helloworlderlang"

append_strings(OrigString,ListOfStrings) ->
    solveme.

% same as above, but use foldr and put the OrigString at the end
%
% 39> list_probs:append_strings2("hello",["world","erlang"]).
% "worlderlanghello"

append_strings2(OrigString,ListOfStrings) ->
    solveme.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PART B: LIST COMPREHENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% For these problems, use list comprehensions NOT the lists functions

% given a list of tuples {A,B} swaps their order so it becomes {B,A}
%
% 19> list_probs:swap_tup([{a,b},{3,foo},{bar,"hello"}]).
% [{b,a},{foo,3},{"hello",bar}]

swap_tup(List) ->
    solveme.

% returns all the even members of a list
%
% 13> list_probs:evens([1,2,3,4,5,6]).
% [2,4,6]

evens(List) ->
    solveme.

% returns a set of tuples with all pairs that sum to 10
%
% 17> list_probs:sum_to_10([1,2,3,4,5,6,7]).
% [{3,7},{4,6},{5,5},{6,4},{7,3}]

sum_to_10(List) ->
    solveme.

% the first list is a set of tupes of the form {Key,Value}
% the second list is a set of keys from the first list
% returns a list of values
%
% 26> list_probs:get_values([{a,1},{b,2},{c,3},{d,4}],[c,a]).
% [3,1]

get_values(Map,KeysToGet) ->
    solveme.
