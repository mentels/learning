-module(no_specs).
-export([lists1/0, lists2/0, tail_rec/0]).

lists1() ->
    expect_empty([ala]).

lists2() ->
    expect_nonempty([]).

tail_rec() ->
    tail_rec(1).

%% Lists

expect_empty([] = L) ->
    [head | L].

expect_nonempty([H]) ->
    [H | [1,2,3]].

tail_rec(A) ->
    case A of 
        X when X > 0 ->
            tail_rec(A+1);
        _ ->
            ok
    end.
    
