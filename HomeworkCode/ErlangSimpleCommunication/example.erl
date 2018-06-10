-module(example).
-export([buffalo_counter/1,spawn_counter/0]).

buffalo_counter(CurrentNum) ->
    receive
        print ->
            io:format("~w~n",[CurrentNum]),
            buffalo_counter(CurrentNum);
        {add,Value} ->
            io:format("~w+~w=~w~n",[CurrentNum,Value,CurrentNum+Value]),
            buffalo_counter(CurrentNum+Value);
        exit ->
            ok
                
    end.

spawn_counter() ->
    spawn(fun() -> buffalo_counter(0) end).

% send commands like this
% Pid = example:spawn_counter().
% Pid ! print.
% Pid ! {add,5}.
% Pid ! exit.
