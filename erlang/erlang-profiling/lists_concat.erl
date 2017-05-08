-module(lists_concat).

-compile(export_all).

run(N) ->
    [begin 
         io:format("=====> ~p~n", [Method]),
         apply(?MODULE, Method, [N])
     end || Method <- [plus_plus, 
                       flatten, 
                       append_lists, 
                       append_lists_of_lists]].

plus_plus(N) ->
    profile(fun()-> random_list(N) ++ random_list(N) end).

flatten(N) ->
    profile(fun()-> lists:flatten([random_list(N), random_list(N)]) end).

append_lists(N) ->
    profile(fun() -> lists:append(random_list(N), random_list(N)) end).

append_lists_of_lists(N) ->
    profile(fun() -> lists:append([random_list(N), random_list(N)]) end).

random_list(N) ->
    [rand:uniform() || _ <- lists:seq(1, N)].

profile(Fun) ->
    eprof:start(),
    eprof:profile(Fun),
    eprof:analyze(),
    eprof:stop().
