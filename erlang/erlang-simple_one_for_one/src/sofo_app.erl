%%%-------------------------------------------------------------------
%% @doc sofo public API
%% @end
%%%-------------------------------------------------------------------

-module(sofo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Port = application:get_env(sofo, port, 5566),
    AllowedReqs = application:get_env(sofo, allowed_reqs, 50),
    sofo_sup:start_link(Port, AllowedReqs).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
