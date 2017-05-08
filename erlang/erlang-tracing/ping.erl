-module(ping).
-export([start/0, send/1, loop/0, trace_calls/1]).

trace_calls(TrueOrFalse) ->
    erlang:trace(all, TrueOrFalse, [call]).

start() -> spawn_link(ping, loop, []).

send(Pid) ->
    Pid ! {self(), ping},
    receive  pong -> pong end.

loop() ->
    receive
        {Pid, ping} -> 
            spawn(mod, non_existing_fun, []),
            Pid ! pong,
            loop()
    end.
