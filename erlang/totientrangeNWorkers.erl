%% Daniel Brown 2469283B

-module(totientrangeNWorkers).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
	 sumTotient/2,
     start_server/0,
     worker/1,
     server/4
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
  


%% Helper function that spawns n number of workers

% spawn_workers(1) ->
%     register("worker" ++ integer_to_list(1), spawn(totientrangeNWorkers, worker, [1] )).

spawn_workers(N) ->
    io:format("Spawning worker ~p ~n",[N]),
    if 
        N /= 1 ->
            register(list_to_atom("worker" ++ integer_to_list(N)), spawn(totientrangeNWorkers, worker, [N] )),
            spawn_workers(N-1);
        N == 1 ->
            register(list_to_atom("worker" ++ integer_to_list(N)), spawn(totientrangeNWorkers, worker, [N] ))
    end.

%% Instead of the server sending the messages directly in its function, 
%% it calls this helper function to distribute the work.
%% Funny note: Workers do work in backwards order. i.e. Worker N does the first batch, and worker 1 does the last.
%% This is due to simplicity of implementation with the recursive basecase, but isn't very intuitive...

give_work(range, Lower, Interval, N) ->
    Upper = Lower + (Interval),
    if
        N /=1 ->
            list_to_atom("worker" ++ integer_to_list(N)) ! {range, Lower, Upper},
            give_work(range, Upper+1, Interval, N-1);
        N == 1 ->
            list_to_atom("worker" ++ integer_to_list(1)) ! {range, Lower, Upper}
    end.


%% Helper function to kill workers. Note that when the last worker is killed it finishes the server also.
% kill_workers(1) ->
%     "worker" ++ integer_to_list(1) ! {finished},
%     server ! {finished}.

kill_workers(N) ->
    list_to_atom("worker" ++ integer_to_list(N)) ! finished,
    if 
        N /= 1 ->
            kill_workers(N-1);
        N == 1 ->
            server ! finished
    end.


% Worker process
worker(N) ->
    
    receive
        finished -> 
            io:format("Worker~p: Finished~n",[N]);
        {range, Lower, Upper} ->
            io:format("worker~p: Started~n", [N]),
            io:format("worker~p: Computing Range ~p ~p ~n", [N, Lower, Upper]),
            Res = sumTotient(Lower,Upper),
            server ! {result, Res},
            worker(N) %% just starting the worker up for it to be killed. Seems redundant but necessary for desired workflow...  
    end.

%% Server Process. Holds list of sums as parameter and number of workers N (when spawned N=0)
server(S, US, Sums, N) ->
    receive 
        finished ->
            start_server(); %% essentially restart the server so that when doing timings you don't need to restart manually...

        {range, Lower, Upper, NumThreads} ->
            io:format("Server: Started ~n",[]),
            io:format("Server: Worker request received...~n",[]),
            {_, StartS, StartUS} = os:timestamp(),
            spawn_workers(NumThreads),
            Dif = Upper - Lower,
            Interval = trunc(Dif / NumThreads),
            give_work(range, Lower, Interval, NumThreads),
            server(StartS, StartUS, Sums, NumThreads); % Reset the server state to recieve again and set N correctly 

        {result, Res} -> 
            io:format("Server: Received Sum ~p~n",[Res]),
            New_Sums = [Res|Sums],
            Len_Sums = length(New_Sums),
            if 
                Len_Sums == N -> % If the number of sums = the number of threads, we can sum the total and begin the masacre
                    Final_Res = lists:foldl(fun(X, Sum) -> X + Sum end, 0, New_Sums),
                    io:format("Server: Sum of totients: ~p~n",[Final_Res]),
                    printElapsed(S, US),
                    kill_workers(N);
                Len_Sums < N ->
                    server(S, US, New_Sums, N)
            end
    end.


start_server() ->
    register(server, spawn(totientrangeNWorkers, server, [0, 0, [], 0])).