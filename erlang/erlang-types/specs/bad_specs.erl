-module(bad_specs).
-export([lists1/0, lists2/0, maps/0]).

%% Lists

lists1() ->
    expect_empty2([ala]).

lists2() ->
    expect_nonempty2([]).

-spec expect_empty2([]) -> any().
expect_empty2(List) ->
    [head | List].

-spec expect_nonempty2([atom(), ...]) -> any().
expect_nonempty2(List) ->
    [head | List].
    

%% Maps

%% Dialyzer is not detecting this if < 19.0

maps() ->
    empty_map(#{ala => ola}),
    at_least_key(#{key => "ala", another_key => "val"}),
    at_least_key2(#{key => "ala", "another_key" => "val"}).

-spec empty_map(#{}) -> map().
empty_map(M) ->
    maps:put(key, val, M).

-spec at_least_key(#{key => atom()}) -> map().
at_least_key(#{key := _} = M) ->
    maps:remove(key, M).

-spec at_least_key2(#{atom() => atom()}) -> #{}.
at_least_key2(#{key := V} = M) ->
    maps:put(another_key, V, M).
