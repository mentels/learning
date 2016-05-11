-module(profile).

-compile(export_all).

%% timer:tc/3 measures the elapsed real time as reported by
%% os:timestamp/0
with_tc(N) ->
    {TimeMicros, _Result} = timer:tc(factorial, compute, [N]),
    TimeMicros.

%% erlang:statistics/1 returns time in milliseconds
with_statistics(N) ->
    {_TotalRunTime, _TimseSinceLastCall} = erlang:statistics(runtime),
    factorial:compute(N),
    element(2, erlang:statistics(runtime)).

with_cprof(N) ->
    cprof:start(),
    factorial:compute(N),
    cprof:pause(),
    Analysis = cprof:analyse(factorial),
    cprof:stop(),
    Analysis.

with_fprof(N) ->
    fprof:trace(start),
    factorial:compute(N),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse().
    %% io:format("~s~n", [element(2, file:read_file("fprof.analysis"))]).
%% [file:delete(F) || F <- filelib:wildcard("fprof.*")].

with_fprof_and_new_proc(N) ->
    fprof:trace(start),
    spawn(fun() -> factorial:compute(N) end),
    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse().

with_eprof(N) ->
    eprof:start(),
    eprof:profile(fun() -> factorial:compute(N) end),
    eprof:analyze(),
    eprof:stop().


