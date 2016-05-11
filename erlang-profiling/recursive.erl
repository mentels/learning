-module(recursive).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

body_rec(N) ->
    Mapper = fun(X) -> math:pow(X*2, 2)/math:pi() end,
    body_rec_mapper(lists:seq(1, N), Mapper).

tail_rec(N) ->
    Mapper = fun(X) -> math:pow(X*2, 2)/math:pi() end,
    tail_rec_mapper(lists:seq(1, N), Mapper, []).

body_rec_mapper([], _) ->
    [];
body_rec_mapper([H|T], Mapper) ->
    [Mapper(H) | body_rec_mapper(T, Mapper)].

tail_rec_mapper([], _, Acc) ->
    lists:reverse(Acc);
tail_rec_mapper([H|T], Mapper, Acc) ->
    tail_rec_mapper(T, Mapper, [Mapper(H)|Acc]).

profile_body_rec(statistics, N) ->
    prof_with_statistics(fun() -> body_rec(N) end);
profile_body_rec(eprof, N) ->
    prof_with_eprof(fun() -> body_rec(N) end).

profile_tail_rec(statistics, N) ->
    prof_with_statistics(fun() -> tail_rec(N) end);
profile_tail_rec(eprof, N) ->
    prof_with_eprof(fun() -> tail_rec(N) end).

prof_with_statistics(Fn) ->
    erlang:statistics(runtime),
    Fn(),
    element(2, erlang:statistics(runtime)).

prof_with_eprof(Fn) ->
    eprof:start(),
    eprof:profile(Fn),
    eprof:analyze(),
    eprof:stop().

sanity_test_() ->
    {timeout, 10,
     [
      fun() ->
              [?assert(body_rec(N) =:= tail_rec(N))
               || N <- lists:seq(1, random:uniform(1000))]
      end
     ]}.
