-module(roulette).
-export([loop/0, spawn_fail_on_death/0, spawn_restart_on_death/0]).

% send a number, 1-6
loop() ->
    receive
        3 -> io:format("bang.~n"), exit(roulette_death);
        _ -> io:format("click~n"), loop()
    end.

fail_on_death(Pid) ->
    link(Pid),
    receive
        Anything -> io:format("Got message: ~w~n",[Anything])
    end,
    fail_on_death(Pid).

spawn_fail_on_death() ->
    Pid1 = spawn(fun() -> loop() end),
    Pid2 = spawn(fun() -> fail_on_death(Pid1) end),
    {Pid1, Pid2}.

%% You can see this work like this:
%% 12> {A,B} = roulette:spawn_fail_on_death().
%% {<0.74.0>,<0.75.0>}
%% 13> B ! foobar.
%% Got message: foobar
%% foobar
%% 14> A ! 3.
%% bang.
%% 3
%% 15> A ! foobar.
%% foobar

restart_on_death(Pid) ->
    process_flag(trap_exit, true),
    link(Pid),
    receive
        {'EXIT',_,roulette_death} ->
            io:format("Roulette death - restarting~n"),
            restart_on_death(spawn(fun() -> loop() end));
        {'EXIT',_,UnknownError} ->
            exit(UnknownError);
        Anything -> 
            io:format("Got message: ~w~n",[Anything]),
            restart_on_death(Pid)
    end.


spawn_restart_on_death() ->
    Pid1 = spawn(fun() -> loop() end),
    Pid2 = spawn(fun() -> restart_on_death(Pid1) end),
    {Pid1, Pid2}.


%% 21> {C,D} = roulette:spawn_restart_on_death().
%% {<0.100.0>,<0.101.0>}
%% 22> C ! 2.
%% click
%% 2
%% 23> C ! 3.
%% bang.
%% 3
%% Roulette death - restarting
%% 24> D ! foo.
%% Got message: foo
%% foo
