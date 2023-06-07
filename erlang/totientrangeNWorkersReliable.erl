%% Daniel Brown 2469283B


-module(totientrangeNWorkersReliable).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
	 sumTotient/2,
     start_server/0,
     worker/1,
     server/4,
     watcher/3,
     testRobust/2
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
  


%% Helper function that spawns n number of watchers and then sends a message to them to spawn workers
spawn_watchers(N) ->
    register(list_to_atom("watcher" ++ integer_to_list(N)), spawn(totientrangeNWorkersReliable, watcher, [N,0,0])),
    list_to_atom("watcher" ++ integer_to_list(N)) ! {spawn_worker},
    if 
        N /= 1 ->
            spawn_watchers(N-1);
        true ->
            ok
    end.

%% Instead of the server sending the work, now the watchers do it. 
%% Watcher is given a message with the upper and lower bounds for its worker. 

give_work(range, Lower, Interval, N) ->
    Upper = Lower + (Interval),
    list_to_atom("watcher" ++ integer_to_list(N)) ! {range, Lower, Upper},
    if
        N /=1 ->
            give_work(range, Upper+1, Interval, N-1);
        true ->
            ok
    end.

% Watcher process
watcher(N, Lower, Upper) ->
    process_flag(trap_exit, true),
    receive
        finished ->
            io:format("Watcher~p: Finished~n",[N]);
        {spawn_worker} ->
            register(list_to_atom("worker" ++ integer_to_list(N)), spawn(totientrangeNWorkersReliable, worker, [N] )),
            io:format("Watcher~p: Watching Worker~p~n",[N,N]),
            watcher(N, Lower, Upper);

        % assign work to worker and make watcher now have a correct 
        % Upper and Lower count so that it can restart worker correctly
        {range, IncLower, IncUpper} -> 
            list_to_atom("worker" ++ integer_to_list(N)) ! {range, IncLower, IncUpper},
            watcher(N, IncLower, IncUpper);

        {'EXIT', From, Reason} ->
            io:format("Watcher: ~p killed by ~p.  Restarting. ~n", [From, Reason]),
            register(list_to_atom("worker" ++ integer_to_list(N)), spawn(totientrangeNWorkersReliable, worker, [N] )),
            list_to_atom("worker" ++ integer_to_list(N)) ! {range, Lower, Upper},
            watcher(N, Lower, Upper)
    end.
            


% Worker process. Note it is not reset to be killed when completed since after completion, if the chaos monkey kills it,
% it may recalculate causing even more, well... Chaos.
worker(N) ->
    
    Watcher_PID = whereis(list_to_atom("watcher" ++ integer_to_list(N))),
    link(Watcher_PID),  
    receive
        {range, Lower, Upper} ->
            io:format("worker~p: Computing Range ~p ~p ~n", [N, Lower, Upper]),
            Res = sumTotient(Lower,Upper),
            server ! {result, Res},
            list_to_atom("watcher" ++ integer_to_list(N)) ! finished % just let the worker kill its watcher its done
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
            spawn_watchers(NumThreads),
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
                    printElapsed(S, US);
                Len_Sums < N ->
                    server(S, US, New_Sums, N)
            end
    end.


start_server() ->
    register(server, spawn(totientrangeNWorkersReliable, server, [0, 0, [], 0])).



workerName(WorkerNum)  ->
    list_to_atom("worker" ++ integer_to_list(WorkerNum)).

workerChaos(NWorkers,NVictims) ->
    lists:map(
        fun( _ ) ->
            timer:sleep(500), %% Sleep for .5s
%% Choose a random victim
            WorkerNum = rand:uniform(NWorkers),
            io:format("workerChaos killing ~p~n",
                [workerName(WorkerNum)]),
            WorkerPid = whereis(workerName(WorkerNum)),
            if %% Check if victim is alive
                WorkerPid == undefined ->
                    io:format("workerChaos already dead: ~p~n",
                                [workerName(WorkerNum)]);
                true -> %% Kill Kill Kill
                    exit(whereis(workerName(WorkerNum)),chaos)
        end
    end,
    lists:seq( 1, NVictims ) ).

testRobust(NWorkers,NVictims) ->
    ServerPid = whereis(server),
    if 
        ServerPid == undefined ->
            start_server();
        true ->
            ok
    end,
    server ! {range, 1, 15000, NWorkers},
    workerChaos(NWorkers,NVictims).