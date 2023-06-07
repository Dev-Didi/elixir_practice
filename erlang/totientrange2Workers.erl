%% Daniel Brown 2469283B
-module(totientrange2Workers).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
	 sumTotient/2,
     start_server/0,
     worker/0,
     server/3
	]).

hcf(X,0) -> X;
hcf(X,Y) -> hcf(Y,X rem Y).

%% relprime x y = hcf x y == 1

relprime(X,Y) -> 
  V = hcf(X,Y),
  if 
    V == 1 
      -> true;
    true 
      -> false
  end.

%%euler n = length (filter (relprime n) (mkList n))

euler(N) -> 
  RelprimeN = fun(Y) -> relprime(N,Y) end,  
  length (lists:filter(RelprimeN,(lists:seq(1,N)))).

%% Take completion timestamp, and print elapsed time

printElapsed(S,US) ->
  {_, S2, US2} = os:timestamp(),
                       %% Adjust Seconds if completion Microsecs > start Microsecs
  if
    US2-US < 0 ->
      S3 = S2-1,
      US3 = US2+1000000;
    true ->
      S3 = S2,
      US3 = US2
  end,
  io:format("Server: Time taken in Secs, MicroSecs ~p ~p~n",[S3-S,US3-US]).


%%sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])

sumTotient(Lower,Upper) -> 
  lists:sum(lists:map(fun euler/1,lists:seq(Lower, Upper))).
  

% Worker process
worker() ->
    
    receive
        finished -> 
            io:format("Worker: Finished~n",[]);
        {range, Lower, Upper} ->
            io:format("Worker: Started~n", []),
            io:format("worker: Computing Range ~p ~p ~n", [Lower, Upper]),
            Res = sumTotient(Lower,Upper),
            server ! {result, Res},
            worker() %% just starting the worker up for it to be killed. Seems redundant but necessary for desired workflow...  
    end.

%% Server Process. Holds the start time (assigned when given a task) and a list of sums as a parameter 
server(S, US, Sums) ->
    receive 
        finished ->
            start_server(); %% essentially restart the server so that when doing timings you don't need to restart manually...

        {range, Lower, Upper} ->
            io:format("Server: Started ~n",[]),
            io:format("Server: Worker request received...~n",[]),
            {_, StartS, StartUS} = os:timestamp(),
            register(worker1, spawn(totientrange2Workers, worker,[])),
            register(worker2, spawn(totientrange2Workers, worker,[])),
            Half = trunc((Upper - Lower)/2),
            worker1 ! {range, Lower, Half},
            worker2 ! {range, Half+1, Upper},
            server(StartS, StartUS, Sums); % Reset the server state to recieve the results

        {result, Res} -> 
            io:format("Server: Received Sum ~p~n",[Res]),
            New_Sums = [Res|Sums],
            Len_Sums = length(New_Sums),
            io:format("Server: Number of Sums Received: ~p ~n",[Len_Sums]),
            if 
                Len_Sums == 2 -> % If the number of sums = the number of threads, we can sum the total and begin the masacre
                    Final_Res = lists:foldl(fun(X, Sum) -> X + Sum end, 0, New_Sums),
                    io:format("Server: Sum of totients: ~p~n",[Final_Res]),
                    printElapsed(S, US),
                    worker1 ! finished,
                    worker2 ! finished;
                Len_Sums < 2 ->
                    server(S, US, New_Sums)
            end
    end.


start_server() ->
    register(server, spawn(totientrange2Workers, server, [0, 0, []])).