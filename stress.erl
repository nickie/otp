-module(stress).
-export([
	 mklist/1, mktuple/1, mkfunny/1, mkcls/1,
	 mkimfunny1/1, mkimfunny2/1, mkimfunny3/1, mkimfunny4/1, mkimfunny5/1,
	 bench/0, bench/1, bench/2, term_bench/1, regression/2, timer/2
	 ]).


% Functions that create various data structures without sharing

mklist(0) -> 0;
mklist(M) -> X1 = mklist(M-1), X2 = mklist(M-1), [X1, X2].

mktuple(0) -> 0;
mktuple(M) -> X1 = mktuple(M-1), X2 = mktuple(M-1), {X1, X2}.

mkfunny(0) -> [];
mkfunny(M) -> [mktuple(M div 2) | mkfunny(M-1)].

mkcls(0) -> 42;
mkcls(M) -> X1 = mkcls(3*M div 7), X2 = mkcls(4*M div 7),
            X3 = mkcls(5*M div 7), X4 = mkcls(2*M div 7),
            X5 = mkcls(M-1),
            F = fun (N) -> [N, X1, M, X2] end, {X3, F, [M, X4, M | X5]}.

mkimfunny1(0) -> 42;
mkimfunny1(M) -> X1 = mkimfunny1(3*M div 4), X2 = mkimfunny1(2*M div 3),
                 [X1, X2 | mkimfunny1(M-1)].

mkimfunny2(0) -> 42;
mkimfunny2(M) -> X1 = mkimfunny2(2*M div 3), X2 = mktuple(3*M div 4),
                 [X1, X2 | mkimfunny2(M-1)].

mkimfunny3(0) -> 42;
mkimfunny3(M) -> X1 = mktuple(2*M div 7), Y1 = mkimfunny3(M-1),
                 X2 = mktuple(5*M div 7), Y2 = mkimfunny3(M-1),
                 [Y1, X1, Y2 | X2].

mkimfunny4(0) -> {42};
mkimfunny4(M) -> X = mkimfunny4(M-1), [M | X].

mkimfunny5(0) -> {42};
mkimfunny5(M) -> X = mkimfunny5(M-1), Y = mkimfunny5(5*M div 6),
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


% Machinery for benchmarking

bench() -> bench(0).
bench(N) -> run(N, all_tests()).
bench(From, To) -> run(0, lists:sublist(all_tests(), From, To-From+1)).

run(0, [])    -> ok;
run(0, [X|L]) -> timer(X), run(0, L);
run(1, [X|_]) -> timer(X);
run(N, [_|L]) -> run(N-1, L).

timer({N, X}) -> timer(N, X).

timer(N, X) ->
    io:format("Copying ~P, times ~w, ", [X, 3, N]),
    Opts = [],
    %Opts = [{min_heap_size, 100000000}],
    Parent = self(),
    Worker = fun () -> T = the_test(X),
                       Size = erts_debug:flat_size(T),
                       {Time, ok} = timer:tc(fun regression/2, [N, T]),
                       Parent ! {Time, Size}
             end,
    spawn_opt(Worker, Opts),
    receive
	{Time, Size} ->
            io:format("of size ~w, time = ~.6f~n", [Size, Time / 1000000])
    end.

the_test({apply, F, Args}) -> apply(?MODULE, F, Args);
the_test(T) -> T.

term_bench(N) -> {_, X} = lists:nth(N, all_tests()),
                 the_test(X).


% Regression test: copy term T (that does not share anything) N times

regression(N, T) ->
    Opts = [],
    %Opts = [{min_heap_size, 100000000}],
    Parent = self(),
    Child = spawn_opt(fun () -> receiver_aux(Parent, N) end, Opts),
    sender_aux(Child, T, N).

sender_aux(_, _, 0) ->
    receive
        ok -> ok
    end;
sender_aux(Child, X, N) ->
    Child ! X,
    sender_aux(Child, X, N-1).

receiver_aux(Parent, 0) ->
    Parent ! ok;
receiver_aux(Parent, N) ->
    receive
        _ -> receiver_aux(Parent, N-1)
    end.


% The tests

all_tests() ->
    lists:concat([
      % big terms just once
      [{1, X} || X <- [{apply, mklist, [25]},             %size 134217724
                       {apply, mktuple, [25]},            %size 100663293
                       {apply, mkfunny, [50]},            %size 301989829
                       {apply, mkimfunny1, [50]},         %size 129604984
                       {apply, mkimfunny2, [32]},         %size 109176439
                       {apply, mkimfunny3, [24]},         %size 224779584
                       {apply, mkimfunny4, [100000000]},  %size 200000002
                       {apply, mkimfunny5, [72]},         %size 119853322
                       {apply, mkcls, [58]}               %size 248522720
                      ]],
      % really small terms extremely many times
      [{50000000, X} || X <- [42,
                              [],
                              ok,
                              [42],
                              {42},
                              <<>>,
                              <<42>>,
                              <<17, 42>>]],
      % small terms many times
      [{10000000, lists:seq(1, 20)},
       {5000000, mklist(5)},
       {5000000, mktuple(5)},
       {2500000, mkcls(3)},
       {1000000, lists:seq(1, 250)},
       {500000, mklist(8)},
       {500000, mktuple(8)},
       {250000, mkcls(6)}
      ]
    ]).
