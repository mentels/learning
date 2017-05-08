-module(gen_server_system_events).

-behaviour(gen_server).

%% Test API
-export([system_events_custom_fn/0]).

%% API
-export([start_link/1,
         call/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% Test functions (no traping exits)
%%====================================================================

%% TODO: Run all fucntion pringint the config

system_events_custom_fn() ->
    Fn = fun(Cnt, {in, Msg}, _ProcState) ->
                 io:format("Got message no ~p: ~p~n",[Cnt, Msg]),
                 Cnt+1;
            (Cnt, Other, _ProcState) ->
                 io:format("Got other event: ~p~n", [Other]),
                 Cnt
         end,
    start_link([
                [{debug, {install, {Fn, 0}}}]
               ]).

%%====================================================================
%% API functions
%%====================================================================

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], Opts).

call(Server, Fn) ->
    gen_server:call(Server, Fn).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, undefined}.

handle_call(Fn, _From, State) ->
    Fn(),
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Req, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
