%% @todo: write a loop to send messages from any point 
%% of the ring.
-module(ring).
-export([benchmark/2]).

%% run benchmark for N processes and M messages.
benchmark(N, M)	->
    Pids = makering(N),
    for(1, M, fun() -> spawn(fun() -> start(message, Pids) end) end).

%% setting up the process ring for sending
%% messages & return the Pids array
makering(M)	->
    %% create the processes
    Pids = for(1, M, fun() -> spawn(fun() -> loop() end) end),
    [Head | _T] = Pids,
    %% now setup the ring. one simple init loop to make each
    %% each process aware of its next process in the ring
    %% we need to pass Head head here as we want to connect
    %% the last process back to the head.
    connect(Head, Pids),
    %% return the Pids
    Pids.

%% If only two elements are left in the list just point
%% the first process to the next process. And point the
%% the last process to the first process.
connect(Head, [H, N])	->
    H ! {next, N},
    N ! {next, Head};
%% Connect the head and the next element and move to the
%% next elements
connect(Head, [H, N | T])	->	
    H ! {next, N},
    connect(Head, [N | T]).

%% send message from the beggining of the 
%% ring.
start(Mesg, Pids)	->
    [Head|T] = Pids,
    Head ! {lists:last(T), Mesg, length(Pids)}.

%% order a mass suicide for all process nodes	
killall(Pids)	->
    lists:foreach(fun(Pid) -> Pid ! die end, Pids).

%% message waiting loop. waits for message and sends 
%% it to the next process node. commits sepukku if 
%% it is ordered to die.
loop()	->
    receive
        %% now that we have the next pid, we move into the
        %% the listening loop for that function. remember
        %% the loop/1 will share the same Pid as this, so
        %% no worries.
        {next, NextPid} ->
            io:format("Set next for ~p as ~p~n", [self(), NextPid]),
            wait(NextPid)
    end.

wait(NextPid)	->
    receive
        {From, Mesg, NumHops}	when NumHops >= 0 ->
            io:format("~w from ~p to ~p~n", [Mesg, From, self()]),
            NextPid ! {self(), Mesg, NumHops - 1},
            wait(NextPid);
        die -> void
    end.

%% custom for loops
for(N,N,F)	->	[F()];
for(M,N,F)	->  [F()|for(M + 1, N, F)].
