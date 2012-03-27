-module(foo).
-export([vanilla_sz/1, vanilla_sz/2,
         show_optim_destroys_sharing/0,
         show_send_destroys_sharing/0,
         show_printing_may_be_bad/0,
         % show_compiler_crashes/0,
         mklist/1, mktuple/1, mkfunny/1, mkbin/1,
         mkimlist/1, mkimfunny/1, mkimfunny2/1, mkimfunny3/1,
         mkimfunny4/1, mkimfunny5/1, mkcls/1,
         sz/1, sz/2,
         test/0, test/1, test/2, test/3, the_test/1,
         paranoid/1
         ]).


% Function that shows the sizes (flat and shared) of a term

vanilla_sz(T) ->
    vanilla_sz(T, 5).
vanilla_sz(T, P) ->
    io:format("term: ~80P~n", [T, P]),
    F = erts_debug:flat_size(T),
    X = erts_debug:size(T),
    io:format("flat_size = ~B~n", [F]),
    io:format("size = ~B~n", [X]).


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


% This shows that printing may be bad too

show_printing_may_be_bad(N) when N =< 40 ->
    L = mklist(N),
    io:format("size is ~B: ", [erts_debug:size(L)]),
    io:format("~80P~n", [L, 4]),
    show_printing_may_be_bad(N+5).
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
                io:format("size = ~B    BUT   size_shared = ~B~n", [X, Y]),
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
    {Res1, Res2}.


% Machinery for testing

test() -> test(0, fun sz/1).

test(N) when is_integer(N) -> test(N, fun sz/1);
test(Fun) -> test(0, all_tests(), Fun).

test(N, Fun) -> test(N, all_tests(), Fun).

the_test(N) -> lists:nth(N, all_tests()).

test(From, To, Fun) when is_integer(From) andalso is_integer(To) ->
    test(0, lists:sublist(all_tests(), From, To-From+1), Fun);
test(0, [], _) -> ok;
test(0, [T], Fun) -> Fun(T);
test(0, [T|L], Fun) -> Fun(T), io:format("~n", []), test(0, L, Fun);
test(1, [T|_], Fun) -> Fun(T);
test(N, [_|L], Fun) -> test(N-1, L, Fun).


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


% The tests

all_tests() ->
    L0 = [1, 2, 3, 4, 5, 6, 7, 8],
    L1 = [L0, L0, L0, L0],
    T1 = {L1, L0, [L1, L1, L0], {L0, [L1, L1]}},
    B1 = <<1,2,3,4>>,
    B2 = <<5,6,B1/binary,7,8>>,
    B3 = <<B1/binary, B2/binary, B2/binary>>,
    T2 = {B1, [B1, B1], B1},
    B4 = mkbin(10),
    [L0, L1, T1,
     mklist(10), mktuple(10), mkfunny(10),
     mkimfunny(20), mkimfunny2(20), mkimfunny3(20), mkimfunny4(1000),
     mkimfunny5(50), mkcls(10),
     B1, B2, B3, T2, B4
    ].
