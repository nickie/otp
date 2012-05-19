-module(heir).
-export([heir/0, give_away/0]).

-define(line,).

heir() ->
    erts_debug:set_internal_state(available_internal_state, true),
    start_spawn_logger(),
    wait_for_test_procs(),
    heir_do([]),
    wait_for_test_procs(true),
    stop_spawn_logger().

heir_do(Opts) ->
    ?line EtsMem = etsmem(),
    Master = self(),

    %% Different types of heir data and link/monitor relations
    TestFun = fun(Arg) -> {EtsMem,Arg} end,
    Combos = [{Data,Mode} || Data<-[foo_data, <<"binary">>, 
				    lists:seq(1,10), {17,TestFun,self()},
				    "The busy heir"],
			     Mode<-[none,link,monitor]],
    ?line lists:foreach(fun({Data,Mode})-> heir_1(Data,Mode,Opts) end,
			Combos),    			
			 
    %% No heir
    {Founder1,MrefF1} = spawn_monitor(fun()->heir_founder(Master,foo_data,Opts)end),
    Founder1 ! {go, none},
    ?line {"No heir",Founder1} = receive_any(),
    ?line {'DOWN', MrefF1, process, Founder1, normal} = receive_any(),
    ?line undefined = ets:info(foo),

    %% An already dead heir
    {Heir2,MrefH2} = spawn_monitor(fun()->die end),
    ?line {'DOWN', MrefH2, process, Heir2, normal} = receive_any(),
    {Founder2,MrefF2} = spawn_monitor(fun()->heir_founder(Master,foo_data,Opts)end),
    Founder2 ! {go, Heir2},
    ?line {"No heir",Founder2} = receive_any(),
    ?line {'DOWN', MrefF2, process, Founder2, normal} = receive_any(),
    ?line undefined = ets:info(foo),

    %% When heir dies before founder
    {Founder3,MrefF3} = spawn_monitor(fun()->heir_founder(Master,"The dying heir",Opts)end),
    {Heir3,MrefH3} = spawn_monitor(fun()->heir_heir(Founder3)end),
    Founder3 ! {go, Heir3},
    ?line {'DOWN', MrefH3, process, Heir3, normal} = receive_any(),
    Founder3 ! die_please,
    ?line {'DOWN', MrefF3, process, Founder3, normal} = receive_any(),
    ?line undefined = ets:info(foo),

    %% When heir dies and pid reused before founder dies
    NextPidIx = erts_debug:get_internal_state(next_pid),
    {Founder4,MrefF4} = spawn_monitor(fun()->heir_founder(Master,"The dying heir",Opts)end),
    {Heir4,MrefH4} = spawn_monitor(fun()->heir_heir(Founder4)end),
    Founder4 ! {go, Heir4},
    ?line {'DOWN', MrefH4, process, Heir4, normal} = receive_any(),
    erts_debug:set_internal_state(next_pid, NextPidIx),
    {Heir4,MrefH4_B} = spawn_monitor_with_pid(Heir4, 
					      fun()-> ?line die_please = receive_any() end),
    Founder4 ! die_please,
    ?line {'DOWN', MrefF4, process, Founder4, normal} = receive_any(),
    Heir4 ! die_please,
    ?line {'DOWN', MrefH4_B, process, Heir4, normal} = receive_any(),
    ?line undefined = ets:info(foo), 

    ?line verify_etsmem(EtsMem).

heir_founder(Master, HeirData, Opts) ->    
    ?line {go,Heir} = receive_any(),
    HeirTpl = case Heir of
		  none -> {heir,none};
		  _ -> {heir, Heir, HeirData}
	      end,
    ?line T = ets:new(foo,[named_table, private, HeirTpl | Opts]),
    ?line true = ets:insert(T,{key,1}),
    ?line [{key,1}] = ets:lookup(T,key),
    Self = self(),
    ?line Self = ets:info(T,owner),
    ?line case ets:info(T,heir) of
	      none ->
		  ?line true = (Heir =:= none) orelse (not is_process_alive(Heir)),
		  Master ! {"No heir",self()};
	      
	      Heir -> 
		  ?line true = is_process_alive(Heir),
		  Heir ! {table,T,HeirData},
		  die_please = receive_any()
	  end.


heir_heir(Founder) ->
    heir_heir(Founder, none).
heir_heir(Founder, Mode) ->
    ?line {table,T,HeirData} = receive_any(),
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(T,key)),
    ?line case HeirData of
	      "The dying heir" -> exit(normal);
	      _ -> ok
	  end,

    ?line Mref = case Mode of
		     link -> process_flag(trap_exit, true),
			     link(Founder);			      
		     monitor -> erlang:monitor(process,Founder);
		     none -> ok
		 end,
    ?line Founder ! die_please,
    ?line Msg = case HeirData of
		    "The busy heir" -> receive_any_spinning();
		    _ -> receive_any()
		end,
    ?line {'ETS-TRANSFER', T, Founder, HeirData} = Msg,
    ?line foo = T,
    ?line Self = self(),
    ?line Self = ets:info(T,owner),
    ?line Self = ets:info(T,heir),
    ?line [{key,1}] = ets:lookup(T,key),
    ?line true = ets:insert(T,{key,2}),
    ?line [{key,2}] = ets:lookup(T,key),
    ?line case Mode of % Verify that EXIT or DOWN comes after ETS-TRANSFER
	      link -> 
		  {'EXIT',Founder,normal} = receive_any(),
		  process_flag(trap_exit, false);
	      monitor -> 
		  {'DOWN', Mref, process, Founder, normal} = receive_any();
	      none -> ok
	  end.


heir_1(HeirData,Mode,Opts) ->
    io:format("test with heir_data = ~p\n", [HeirData]),
    Master = self(),
    ?line Founder = spawn_link(fun() -> heir_founder(Master,HeirData,Opts) end),
    io:format("founder spawned = ~p\n", [Founder]),
    ?line {Heir,Mref} = spawn_monitor(fun() -> heir_heir(Founder,Mode) end),
    io:format("heir spawned = ~p\n", [{Heir,Mref}]),
    ?line Founder ! {go, Heir},
    ?line {'DOWN', Mref, process, Heir, normal} = receive_any().

give_away() ->
    erts_debug:set_internal_state(available_internal_state, true),
    start_spawn_logger(),
    wait_for_test_procs(),
    give_away_do([]),
    wait_for_test_procs(true),
    stop_spawn_logger().

give_away_do(Opts) ->
    ?line T = ets:new(foo,[named_table, private | Opts]),
    ?line true = ets:insert(T,{key,1}),
    ?line [{key,1}] = ets:lookup(T,key),
    Parent = self(),

    %% Give and then give back
    ?line {Receiver,Mref} = spawn_monitor(fun()-> give_away_receiver(T,Parent) end),
    ?line give_me = receive_any(),
    ?line true = ets:give_away(T,Receiver,here_you_are),
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(T,key)),
    ?line Receiver ! give_back,
    ?line {'ETS-TRANSFER',T,Receiver,"Tillbakakaka"} = receive_any(),
    ?line [{key,2}] = ets:lookup(T,key),
    ?line {'DOWN', Mref, process, Receiver, normal} = receive_any(),

    %% Give and then let receiver keep it
    ?line true = ets:insert(T,{key,1}),
    ?line {Receiver3,Mref3} = spawn_monitor(fun()-> give_away_receiver(T,Parent) end),
    ?line give_me = receive_any(),
    ?line true = ets:give_away(T,Receiver3,here_you_are),
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(T,key)),
    ?line Receiver3 ! die_please,
    ?line {'DOWN', Mref3, process, Receiver3, normal} = receive_any(),
    ?line undefined = ets:info(T),

    %% Give and then kill receiver to get back
    ?line T2 = ets:new(foo,[private | Opts]),
    ?line true = ets:insert(T2,{key,1}),
    ?line ets:setopts(T2,{heir,self(),"Som en gummiboll..."}),
    ?line {Receiver2,Mref2} = spawn_monitor(fun()-> give_away_receiver(T2,Parent) end),
    ?line give_me = receive_any(),
    ?line true = ets:give_away(T2,Receiver2,here_you_are),
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(T2,key)),
    ?line Receiver2 ! die_please,
    ?line {'ETS-TRANSFER',T2,Receiver2,"Som en gummiboll..."} = receive_any(),
    ?line [{key,2}] = ets:lookup(T2,key),
    ?line {'DOWN', Mref2, process, Receiver2, normal} = receive_any(),

    %% Some negative testing
    ?line {'EXIT',{badarg,_}} = (catch ets:give_away(T2,Receiver,"To a dead one")),
    ?line {'EXIT',{badarg,_}} = (catch ets:give_away(T2,self(),"To myself")),
    ?line {'EXIT',{badarg,_}} = (catch ets:give_away(T2,"not a pid","To wrong type")),

    ?line true = ets:delete(T2),
    ?line {ReceiverNeg,MrefNeg} = spawn_monitor(fun()-> give_away_receiver(T2,Parent) end),
    ?line give_me = receive_any(),
    ?line {'EXIT',{badarg,_}} = (catch ets:give_away(T2,ReceiverNeg,"A deleted table")),

    ?line T3 = ets:new(foo,[public | Opts]),
    spawn_link(fun()-> {'EXIT',{badarg,_}} = (catch ets:give_away(T3,ReceiverNeg,"From non owner")),
		       Parent ! done
	       end),
    ?line done = receive_any(),
    ?line ReceiverNeg ! no_soup_for_you,
    ?line {'DOWN', MrefNeg, process, ReceiverNeg, normal} = receive_any(),
    ok.

give_away_receiver(T, Giver) ->
    ?line {'EXIT',{badarg,_}} = (catch ets:lookup(T,key)),
    ?line Giver ! give_me,
    ?line case receive_any() of
	      {'ETS-TRANSFER',T,Giver,here_you_are} ->
		  ?line [{key,1}] = ets:lookup(T,key),
		  ?line true = ets:insert(T,{key,2}),
		  ?line case receive_any() of
			    give_back ->
				?line true = ets:give_away(T,Giver,"Tillbakakaka"),
				?line {'EXIT',{badarg,_}} = (catch ets:lookup(T,key));
			    die_please ->
				ok
			end;
	      no_soup_for_you ->
		  ok
	  end.

%---------------------------------------------------------------------

etsmem() ->
    wait_for_memory_deallocations(),

    AllTabs = lists:map(fun(T) -> {T,ets:info(T,name),ets:info(T,size),
                                   ets:info(T,memory),ets:info(T,type)}
                        end, ets:all()),

    EtsAllocInfo = erlang:system_info({allocator,ets_alloc}),
    ErlangMemoryEts = try erlang:memory(ets) catch error:notsup -> notsup end,

    Mem =
    {ErlangMemoryEts,
     case EtsAllocInfo of
         false -> undefined;
         MemInfo ->
             CS = lists:foldl(
                    fun ({instance, _, L}, Acc) ->
                            {value,{_,SBMBCS}} = lists:keysearch(sbmbcs, 1, L),
                            {value,{_,MBCS}} = lists:keysearch(mbcs, 1, L),
                            {value,{_,SBCS}} = lists:keysearch(sbcs, 1, L),
                            [SBMBCS,MBCS,SBCS | Acc]
                    end,
                    [],
                    MemInfo),
             lists:foldl(
               fun(L, {Bl0,BlSz0}) ->
                       {value,{_,Bl,_,_}} = lists:keysearch(blocks, 1, L),
                       {value,{_,BlSz,_,_}} = lists:keysearch(blocks_size, 1, L),
                       {Bl0+Bl,BlSz0+BlSz}
               end, {0,0}, CS)
     end},
     {Mem,AllTabs}.

verify_etsmem({MemInfo,AllTabs}) ->
    wait_for_test_procs(),
    case etsmem() of
        {MemInfo,_} ->
            io:format("Ets mem info: ~p", [MemInfo]),
            case MemInfo of
                {ErlMem,EtsAlloc} when ErlMem == notsup; EtsAlloc == undefined ->
                    %% Use 'erl +Mea max' to do more complete memory leak testing.
                    {comment,"Incomplete or no mem leak testing"};
                _ ->
                    ok
            end;
        {MemInfo2, AllTabs2} ->
            io:format("Expected: ~p", [MemInfo]),
            io:format("Actual:   ~p", [MemInfo2]),
            io:format("Changed tables before: ~p\n",[AllTabs -- AllTabs2]),
            io:format("Changed tables after: ~p\n", [AllTabs2 -- AllTabs]),
            exit(fail)
    end.

receive_any() ->
    receive M ->
            io:format("Process ~p got msg ~p\n", [self(),M]),
            M
    end.

receive_any_spinning() ->
    receive_any_spinning(1000000).
receive_any_spinning(Loops) ->
    receive_any_spinning(Loops,Loops,1).
receive_any_spinning(Loops,0,Tries) ->
    receive M ->
            io:format("Spinning process ~p got msg ~p after ~p tries\n", [self(),M,Tries]),
            M
    after 0 ->
            receive_any_spinning(Loops, Loops, Tries+1)
    end;
receive_any_spinning(Loops, N, Tries) when N>0 ->
    receive_any_spinning(Loops, N-1, Tries).

spawn_monitor_with_pid(Pid, Fun) when is_pid(Pid) ->
    spawn_monitor_with_pid(Pid, Fun, 1, 10).

spawn_monitor_with_pid(Pid, Fun, N, M) when N > M*10 ->
    spawn_monitor_with_pid(Pid, Fun, N, M*10);
spawn_monitor_with_pid(Pid, Fun, N, M) ->
    ?line false = is_process_alive(Pid),
    case spawn(fun()-> case self() of
                           Pid -> Fun();
                           _ -> die
                       end
               end) of
        Pid ->
            {Pid, erlang:monitor(process, Pid)};
        Other ->
            case N rem M of
                0 -> io:format("Failed ~p times to get pid ~p (current = ~p)\n", [N,Pid,Other]);
                _ -> ok
            end,
            spawn_monitor_with_pid(Pid,Fun,N+1,M)
    end.

wait_for_memory_deallocations() ->
    try
        erts_debug:set_internal_state(wait, deallocations)
    catch
        error:undef ->
            erts_debug:set_internal_state(available_internal_state, true),
            wait_for_memory_deallocations()
    end.

wait_for_test_procs() ->
    wait_for_test_procs(false).

wait_for_test_procs(Kill) ->
    ets_test_spawn_logger ! {sync_test_procs, Kill, self()},
    receive test_procs_synced -> ok end.

%----------------------------------------------------------------------

spawn_logger(Procs) ->
    receive
        {new_test_proc, Proc} ->
            spawn_logger([Proc|Procs]);
        {sync_test_procs, Kill, From} ->
            lists:foreach(fun (Proc) when From == Proc ->
                                  ok;
                              (Proc) ->
                                  Mon = erlang:monitor(process, Proc),
                                  receive
                                      {'DOWN', Mon, _, _, _} ->
                                          ok
                                  after 0 ->
                                          case Kill of
                                              true -> exit(Proc, kill);
                                              _ -> ok
                                          end,
                                          erlang:display({"Waiting for 'DOWN' from", Proc,
                                                          process_info(Proc), pid_status(Proc)}),
                                          receive
                                              {'DOWN', Mon, _, _, _} ->
                                                  ok
                                          end
                                  end
                          end, Procs),
            From ! test_procs_synced,
            spawn_logger([From])
    end.

pid_status(Pid) ->
    try
        erts_debug:get_internal_state({process_status, Pid})
    catch
        error:undef ->
            erts_debug:set_internal_state(available_internal_state, true),
            pid_status(Pid)
    end.

start_spawn_logger() ->
    case whereis(ets_test_spawn_logger) of
        Pid when is_pid(Pid) -> true;
        _ -> register(ets_test_spawn_logger,
                      spawn_opt(fun () -> spawn_logger([]) end,
                                [{priority, max}]))
    end.

stop_spawn_logger() ->
    Mon = erlang:monitor(process, ets_test_spawn_logger),
    (catch exit(whereis(ets_test_spawn_logger), kill)),
    receive {'DOWN', Mon, _, _, _} -> ok end,
    ok.
