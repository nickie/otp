-module(demo).
-export([vanilla_sz/1, vanilla_sz/2,
	 show_optim_destroys_sharing/0,
	 show_send_destroys_sharing/0,
	 show_spawn_destroys_sharing_1/0,
	 show_spawn_destroys_sharing_2/0, show_spawn_destroys_sharing_2_aux/2,
	 show_printing_may_be_bad/0,
	 % show_compiler_crashes/0,
	 mklist/1, mktuple/1, mkfunny/1, mkbin/1,
	 mkimlist/1, mkimfunny/1, mkimfunny2/1, mkimfunny3/1,
	 mkimfunny4/1, mkimfunny5/1, mkcls/1, mkbin2/1,
	 bmklist/1, bmktuple/1, bmkfunny/1, bmkbin/1,
	 bmkimlist/1, bmkimfunny/1, bmkimfunny2/1, bmkimfunny3/1,
	 bmkimfunny4/1, bmkimfunny5/1, bmkcls/1, bmkbin2/1,
	 sz/1, sz/2,
	 sanity/0, sanity/1, sanity/2, sanity/3, term_sanity/1,
	 bench/0, bench/1, bench/2, bench/3, term_bench/1,
	 paranoid/1,
	 regression/3, regr_copy/1, regr_size/1
	 ]).


% Function that shows the sizes (flat and shared) of a term

vanilla_sz(T) ->
    vanilla_sz(T, 5).
vanilla_sz(T, P) ->
    io:format("term: ~80P~n", [T, P]),
    F = erts_debug:flat_size(T),
    X = erts_debug:size(T),
    io:format("flat_size = ~B~n", [F]),
    io:format("size = ~B~n~n", [X]).


% This shows a problem with code generation or optimization,
% which somehow destroys sharing

make_shared_wrong() ->
    L0 = [1, 2, 3, 4, 5],
    L1 = [L0, 1, L0, 2, L0, 3, L0],
    L2 = [L1, 1, L1, 2, L0, 3, L1, 4, L1],
    L2.

make_shared_right() ->
    L0 = [1, 2, 3, 4, 5],
    make_shared_right(L0).
make_shared_right(L0) ->
    L1 = [L0, 1, L0, 2, L0, 3, L0],
    L2 = [L1, 1, L1, 2, L0, 3, L1, 4, L1],
    L2.

show_optim_destroys_sharing() ->
    io:format("Why should this~n", []),
    vanilla_sz(make_shared_wrong(), 100),
    io:format("be any different from this?~n", []),
    vanilla_sz(make_shared_right(), 100).

%% show_compiler_crashes() ->
%%     L0 = [0],
%%     L1 = [L0, L0, L0, L0, L0, L0, L0, L0, L0, L0, L0, L0, L0, L0, L0, L0],
%%     L2 = [L1, L1, L1, L1, L1, L1, L1, L1, L1, L1, L1, L1, L1, L1, L1, L1],
%%     L3 = [L2, L2, L2, L2, L2, L2, L2, L2, L2, L2, L2, L2, L2, L2, L2, L2],
%%     L4 = [L3, L3, L3, L3, L3, L3, L3, L3, L3, L3, L3, L3, L3, L3, L3, L3],
%%     L5 = [L4, L4, L4, L4, L4, L4, L4, L4, L4, L4, L4, L4, L4, L4, L4, L4],
%%     L6 = [L5, L5, L5, L5, L5, L5, L5, L5, L5, L5, L5, L5, L5, L5, L5, L5],
%%     L7 = [L6, L6, L6, L6, L6, L6, L6, L6, L6, L6, L6, L6, L6, L6, L6, L6],
%%     L8 = [L7, L7, L7, L7, L7, L7, L7, L7, L7, L7, L7, L7, L7, L7, L7, L7],
%%     L9 = [L8, L8, L8, L8, L8, L8, L8, L8, L8, L8, L8, L8, L8, L8, L8, L8],
%%     L10 = [L9, L9, L9, L9, L9, L9, L9, L9, L9, L9, L9, L9, L9, L9, L9, L9],
%%     L10.


% This shows that sending a message destroys sharing

show_send_destroys_sharing() ->
    L2 = make_shared_right(),
    Pid = spawn(fun () ->
	     receive {X, Parent} ->
		io:format("RECEIVED:~n", []),
		vanilla_sz(X, 100),
		Parent ! ok
	     end
	  end),
    io:format("SENT:~n", []),
    vanilla_sz(L2, 100),
    Pid ! {L2, self()},
    receive
	ok -> ok
    end.


% This shows that spawning a process destroys sharing
% for terms that are in the function's closure

show_spawn_destroys_sharing_1() ->
    L2 = make_shared_right(),
    io:format("IN PARENT:~n", []),
    vanilla_sz(L2, 100),
    Parent = self(),
    spawn(fun () ->
	      io:format("IN SPAWNED:~n", []),
	      vanilla_sz(L2, 100),
	      Parent ! ok
	  end),
    receive
	ok -> ok
    end.


% This shows that spawning a process destroys sharing
% for terms that are passed as the function's arguments

show_spawn_destroys_sharing_2_aux(Parent, L) ->
    io:format("IN SPAWNED:~n", []),
    vanilla_sz(L, 100),
    Parent ! ok.

show_spawn_destroys_sharing_2() ->
    L2 = make_shared_right(),
    io:format("IN PARENT:~n", []),
    vanilla_sz(L2, 100),
    spawn(?MODULE, show_spawn_destroys_sharing_2_aux, [self(), L2]),
    receive
	ok -> ok
    end.


% This shows that printing may be bad too

show_printing_may_be_bad(N) when N =< 40 ->
    L = mklist(N),
    T = now(),
    io:format("size is ~B: ", [erts_debug:size(L)]),
    io:format("~80P, ", [L, 4]),
    D = timer:now_diff(now(), T),
    io:format("time elapsed: ~.3f sec.~n", [D/1000000]),
    show_printing_may_be_bad(N+5);
show_printing_may_be_bad(_) ->
    ok.

show_printing_may_be_bad() ->
    show_printing_may_be_bad(0).


% Auxiliary functions for creating different kinds of data

mklist(0) -> 0;
mklist(M) -> X = mklist(M-1), [X, X].

mktuple(0) -> 0;
mktuple(M) -> X = mktuple(M-1), {X, X}.

mkfunny(0) -> [];
mkfunny(M) -> [mktuple(3) | mkfunny(M-1)].

mkbin(0) -> << >>;
mkbin(M) -> B = mkbin(M-1), <<B/binary, M, B/binary>>.

mkimlist(0) -> 0;
mkimlist(M) -> [M | mkimlist(M-1)].

mkimfunny(0) -> 42;
mkimfunny(M) -> X = mkimfunny(M div 2), [X, X | mkimfunny(M-1)].

mkimfunny2(0) -> 42;
mkimfunny2(M) -> X = mktuple(M div 2), Y = mkimfunny2(M-1), [X, X | Y].

mkimfunny3(0) -> 42;
mkimfunny3(M) -> X = mktuple(M div 2), Y = mkimfunny3(M-1), [Y, X, Y | X].

mkimfunny4(0) -> {42};
mkimfunny4(M) -> X = mkimfunny4(M-1), [M | X].

mkimfunny5(0) -> {42};
mkimfunny5(M) -> X = mkimfunny5(M-1), Y = mkimfunny5(M div 2),
		 case prime(M) of
		     false -> [Y | X];
		     true  -> {M, X}
		 end.

prime(N) when N < 2 -> false;
prime(N) when N =< 3 -> true;
prime(N) when (N rem 6 =:= 1) orelse (N rem 6 =:= 5) -> prime_chk(N, 5);
prime(_) -> false.

prime_chk(N, I) when I*I > N -> true;
prime_chk(N, I) when N rem I =:= 0 -> false;
prime_chk(N, I) when I rem 6 =:= 1 -> prime_chk(N, I+4);
prime_chk(N, I) -> prime_chk(N, I+2).

mkcls(0) -> 42;
mkcls(M) -> X = mkcls(M-1), F = fun (N) -> [N, X, M, X] end, {X, F, F(M)}.

mkbin2(0) -> <<42>>;
mkbin2(M) -> B = mkbin2(M-1),
	     <<X:4, Y:4, Rest/binary>> = B,
	     <<X, B/binary, M, Rest/binary, Y>>.


% The same auxiliary functions, without sharing

bmklist(0) -> 0;
bmklist(M) -> X1 = bmklist(M-1), X2 = bmklist(M-1), [X1, X2].

bmktuple(0) -> 0;
bmktuple(M) -> X1 = bmktuple(M-1), X2 = bmktuple(M-1), {X1, X2}.

bmkfunny(0) -> [];
bmkfunny(M) -> [bmktuple(M div 2) | bmkfunny(M-1)].

bmkbin(0) -> << >>;
bmkbin(M) -> B1 = bmkbin(M-1), B2 = bmkbin(M-1), <<B1/binary, M, B2/binary>>.

bmkimlist(0) -> 0;
bmkimlist(M) -> [M | bmkimlist(M-1)].

bmkimfunny(0) -> 42;
bmkimfunny(M) -> X1 = bmkimfunny(M div 2), X2 = bmkimfunny(M div 2),
		 [X1, X2 | bmkimfunny(M-1)].

bmkimfunny2(0) -> 42;
bmkimfunny2(M) -> X1 = bmktuple(M div 2), X2 = bmktuple(M div 2),
		  Y = bmkimfunny2(M-1), [X1, X2 | Y].

bmkimfunny3(0) -> 42;
bmkimfunny3(M) -> X1 = bmktuple(M div 2), Y1 = bmkimfunny3(M-1),
		  X2 = bmktuple(M div 2), Y2 = bmkimfunny3(M-1),
		  [Y1, X1, Y2 | X2].

bmkimfunny4(0) -> {42};
bmkimfunny4(M) -> X = bmkimfunny4(M-1), [M | X].

bmkimfunny5(0) -> {42};
bmkimfunny5(M) -> X = bmkimfunny5(M-1), Y = bmkimfunny5(M div 2),
		  case prime(M) of
		      false -> [Y | X];
		      true  -> {M, X}
		  end.

bmkcls(0) -> 42;
bmkcls(M) -> X1 = bmkcls(M-1), X2 = bmkcls(M-1), X3 = bmkcls(M-1),
	     X4 = bmkcls(M-1), X5 = bmkcls(M-1),
	     F = fun (N) -> [N, X1, M, X2] end, {X3, F, [M, X4, M, X5]}.

bmkbin2(0) -> <<42>>;
bmkbin2(M) -> B1 = bmkbin2(M-1),
	      B2 = bmkbin2(M-1),
	      <<X:4, Y:4, Rest/binary>> = B1,
	      <<X, B2/binary, M, Rest/binary, Y>>.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% From here on, you need the custom OTP (nickie's playground)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Function that shows the sizes (flat and shared) of a term

sz(T) ->
    sz(T, 5).
sz(TT, P) ->
    io:format("term: ~80P~n", [TT, P]),
    F = erts_debug:flat_size(TT),
    X = erts_debug:size(TT),
    T = erts_debug:copy_shared(TT),
    W = erts_debug:flat_size(T),
    Z = erts_debug:size(T),
    Y = erts_debug:size_shared(T),
    io:format("flat_size = ~B~n", [F]),
    Res1 =
	case X =:= Y of
	    true ->
		io:format("size = size_shared = ~B~n", [X]),
		ok;
	    false ->
		io:format("size = ~B	BUT   size_shared = ~B~n", [X, Y]),
		error
	end,
    Res2 =
	case F =:= W andalso X =:= Z of
	    true ->
		ok;
	    false ->
		io:format("sanity failure, now W=~B and Z=~B~n", [W, Z]),
		error
	end,
    io:format("~n", []),
    {Res1, Res2}.


% Machinery for testing and benchmarking

sanity() -> sanity(0, fun sz/1).

sanity(N) when is_integer(N) -> sanity(N, fun sz/1);
sanity(Fun) -> test(0, all_tests_sharing(), Fun).

sanity(N, Fun) -> test(N, all_tests_sharing(), Fun).

sanity(From, To, Fun) ->
    test(0, lists:sublist(all_tests_sharing(), From, To-From+1), Fun).

term_sanity(N) -> X = lists:nth(N, all_tests_sharing()), the_test(X).


bench() -> bench(0, fun regr_copy/1).

bench(N) when is_integer(N) -> bench(N, fun regr_copy/1);
bench(Fun) -> test(0, all_tests_no_sharing(), Fun).

bench(N, Fun) -> test(N, all_tests_no_sharing(), Fun).

term_bench(N) -> X = lists:nth(N, all_tests_no_sharing()), the_test(X).

bench(From, To, Fun) ->
    test(0, lists:sublist(all_tests_no_sharing(), From, To-From+1), Fun).

test(0, [], _)	    -> ok;
test(0, [X], Fun)   -> T = the_test(X), Fun(T);
test(0, [X|L], Fun) -> T = the_test(X), Fun(T), test(0, L, Fun);
test(1, [X|_], Fun) -> T = the_test(X), Fun(T);
test(N, [_|L], Fun) -> test(N-1, L, Fun).

the_test({apply, F, Args}) -> apply(?MODULE, F, Args);
the_test(T) -> T.


% Paranoid (concurrent) testing for off-heap data

paranoid_tester(T, Sz) ->
    NewSz = erts_debug:flat_size(T),
    case NewSz =:= Sz of
	true  -> paranoid_tester(T, Sz);
	false -> io:format("It failed! ~80P~nsize was ~B~nnow is ~B~n",
			   [T, 5, Sz, NewSz])
    end.

paranoid(T) ->
    Sz = erts_debug:flat_size(T),
    Fun = fun () -> paranoid_tester(T, Sz) end,
    Pids = [spawn(Fun) || _ <- lists:seq(1, 10)],
    timer:sleep(1000),
    X = erts_debug:size_shared(T),
    timer:sleep(1000),
    Y = erts_debug:size_shared(T),
    timer:sleep(1000),
    case X =:= Y of
	true -> ok;
	false -> io:format("sanity error: ~B and ~B~n", [X, Y])
    end,
    lists:foreach(fun (Pid) -> exit(Pid, kill) end, Pids).


% Regression test: copy a term that does not share anything with
% both methods

regression_timer_1(Fun, Args) -> timer:tc(Fun, Args).

regression_timer_2(Fun, Args) ->
    Start = now(),
    Result = Fun(Args),
    Stop = now(),
    Time = timer:now_diff(Stop, Start),
    {Time, Result}.

regression_timer_3(Fun, Args) ->
    Myself = self(),
    Opts = [{min_heap_size, 100000000}],
    %Opts = [],
    spawn_opt(fun () -> Myself ! regression_timer_1(Fun, Args) end, Opts),
    receive
	Result -> Result
    end.

regression(Fun1, Fun2, Args) ->
    {Time1, Res1} = regression_timer_3(Fun1, Args),
    {Time2, Res2} = regression_timer_3(Fun2, Args),
    case Res1 =:= Res2 of
	true -> ok;
	false -> io:format("sanity error: the two results are different~n", [])
    end,
    {name, Name1} = erlang:fun_info(Fun1, name),
    {name, Name2} = erlang:fun_info(Fun2, name),
    io:format("~s: ~10.6f, ~s: ~10.6f, ~6.2f% ~s~n",
	      [Name1, Time1 / 1000000, Name2, Time2 / 1000000,
	       100*abs(Time1-Time2)/Time1,
	       case Time1 < Time2 of
		   true -> "slower";
		   false -> "faster"
	       end]).

regr_copy(T) -> regression(fun erts_debug:copy_object/1,
			   fun erts_debug:copy_shared/1,
			   [T]).

regr_size(T) -> regression(fun erts_debug:flat_size/1,
			   fun erts_debug:size_shared/1,
			   [T]).

% The tests

all_tests_sharing() ->
    L0 = [1, 2, 3, 4, 5, 6, 7, 8],
    L1 = make_shared_right(L0),
    T1 = {L1, L0, [L1, L1, L0], {L0, [L1, L1]}},
    B1 = <<1,2,3,4>>,
    B2 = <<5,6,B1/binary,7,8>>,
    B3 = <<B1/binary, B2/binary, B2/binary>>,
    T2 = {B1, [B1, B1], B1},
    [L0,
     L1,
     T1,
     {apply, mklist, [10]},
     {apply, mktuple, [10]},
     {apply, mkfunny, [10]},
     {apply, mkimfunny, [20]},
     {apply, mkimfunny2, [20]},
     {apply, mkimfunny3, [20]},
     {apply, mkimfunny4, [1000]},
     {apply, mkimfunny5, [50]},
     {apply, mkcls, [10]},
     B1,
     B2,
     B3,
     T2,
     {apply, mkbin, [10]},
     {apply, mkbin2, [10]}
    ].

all_tests_no_sharing() ->
    [{apply, bmklist, [20]},
     {apply, bmktuple, [20]},
     {apply, bmkfunny, [40]},
     {apply, bmkimfunny, [40]},
     {apply, bmkimfunny2, [30]},
     {apply, bmkimfunny3, [15]},
     {apply, bmkimfunny4, [1000000]},
     {apply, bmkimfunny5, [250]},
     {apply, bmkcls, [10]},
     {apply, bmkbin, [24]},
     {apply, bmkbin2, [20]}
    ].
