-module(correct_specs).
-export([lists/0, maps/0]).

%% Lists

lists() ->
    expect_empty1([]),
    expect_empty2([]),
    expect_nonempty1([ala, ola]),
    expect_nonempty2([ala, ola]).

-spec expect_empty1([]) -> any().
expect_empty1(List) ->
    log(expect_empty1, List).

-spec expect_empty2([any()]) -> any().
expect_empty2(List) ->
    log(expect_empty2, List).

-spec expect_nonempty1(nonempty_list(atom())) -> any().
expect_nonempty1(List) ->
    log(expect_nonempty1, List).

-spec expect_nonempty2([atom(), ...]) -> any().
expect_nonempty2(List) ->
    log(expect_nonempty2, List).


%% Maps

maps() ->
    empty_map(#{}),
    at_least_key(#{key => ala, another_key => "val"}),
    at_least_key2(#{key => ala, "another_key" => "val"}).

-spec empty_map(#{}) -> map().
empty_map(M) ->
    maps:put(key, val, M).

-spec at_least_key(#{key => atom()}) -> map().
at_least_key(#{key := _} = M) ->
    maps:remove(key, M).

-spec at_least_key2(#{atom() => atom()}) -> #{}.
at_least_key2(#{key := V} = M) ->
    maps:put(another_key, V, M).

%% Internal

log(FunName, What) ->
    io:format("Got ~p~n in ~p", [FunName, What]).
    

