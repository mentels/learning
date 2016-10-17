%% @doc 
%% From this thread. What's up?
-module(wild_spec).
-export([main/0]).
-type collection(K, V) :: #{K => V}
                        | dict:dict(K, V)
                        | gb_trees:tree(K, V).
-spec lookup(K,F,C) -> V when
      C :: collection(K, V),
      F :: fun((K, C) -> V).
lookup(K, F, C) -> F(K, C).
main() ->
    C = maps:from_list([{a,1},{b,2},{c,3}]),
    lookup(b, fun maps:get/2, C),
    lookup("bad ignored type", fun maps:get/2, C),
    lookup(b, C, fun maps:get/2).
