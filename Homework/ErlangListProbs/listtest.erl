-module(listtest).
-include_lib("eunit/include/eunit.hrl").

% You can run these tests by calling
% listtest:test().

av_1_test() ->
    [3,4,5] = list_probs:add_val([1,2,3],2).

% not worth bothering with print with paren

ra_1_test() ->
    [[3,6],[2],[3,4,5],[]] = list_probs:reverse_all([[6,3],[2],[5,4,3],[]]).

m_1_test() ->
    true = list_probs:member(1,[0,1,2]).
m_2_test() ->
    false = list_probs:member(1,[3,4,5]).

s_1_test() ->
    [1,3,4,5,7,8] = list_probs:subtract([1,2,3,4,5,6,7,8],[2,6]).

as_1_test() ->
    "helloworlderlang" = list_probs:append_strings("hello",["world","erlang"]).

as_2_test() ->
    "worlderlanghello" = list_probs:append_strings2("hello",["world","erlang"]).

st_1_test() ->
    [{b,a},{2,1}] = list_probs:swap_tup([{a,b},{1,2}]).

e_1_test() ->
    [2,4] = list_probs:evens([1,2,3,4]).

sumten_1_test() ->
    [{9,1},{1,9}] = list_probs:sum_to_10([9,3,1]).

gv_1_test() ->
    [3,4] = list_probs:get_values([{a,3},{b,1},{c,4},{d,5}],[a,c]).
