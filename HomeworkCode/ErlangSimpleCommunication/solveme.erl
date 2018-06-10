-module(solveme).
-export([do_part1_computation/2,do_part2_computation/3,part2_loop/0,part1_spawner/2]).

%% Imagine you've got a simple process of 2 parts.

%% Part 1 of the process is some sort of computation.  It can be run in
%% paralell.  

%% Part 2 of the process is strictly serial.

%% Both of these processes take a job name (string) and a job
%% size (int).  Part 2 takes an additional parameter that is the output
%% of Part 1.


%% How to handle this in Erlang?
%% 1.  Write a process part2_loop that waits for requests.  It takes input,
%% does step 2, then goes back to waiting.  If someone passes it and exit
%% command, it shuts down forever.
%% 2.  Start a Part 2 loop process.
%% 3.  Write a version of part1 that takes an input and a PID, processes
%% it, and then passes the result to the Part 2 process.
%% 4.  Write a function spawn_part1(List,Part2Pid) that takes in a list
%% of inputs and starts each of them in their own thread.
%% 5.  Test your cool creation.

%% BONUS OBJECTIVE - make the partone spawner a process in itself, and
%% make the part 2 reply back to the part one 

%% BONUS BONUS OBJECTIVE - make the partone spawner process also
%% respond to a request that gives a job name and returns whether
%% the process is not started, in progress, or finished

% don't modify this function, just call it
do_part1_computation(JobName,JobSize) ->
    io:format("Starting ~w Part 1.~n",[JobName]),
    Random = random:uniform(), % Pretend random is a carefully constructed result
    timer:sleep(trunc(JobSize*1000*Random)), % simulate doing some computation
    io:format("Finished ~w Part 1. (output: ~w)~n",[JobName,Random]),
    Random. % return our pretend result

% don't modify this function, just call it
do_part2_computation(JobName,JobSize,Part1Result) ->
    io:format("Starting ~w Part 2.~n",[JobName]),
    timer:sleep(trunc(JobSize*1000*Part1Result)), % simulate doing some computation
    io:format("Finished ~w Part 2. (output: ~w)~n",[JobName,Part1Result]),
    ok.

part2_loop() ->
    solveme.

% run this like this:
% solveme:part1_spawner([{job1,1},{job2,1},{job3,0.5}],Part2Pid).
part1_spawner(List,Pid) ->
    solveme.
                                                      
