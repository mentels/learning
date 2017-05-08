%%%-------------------------------------------------------------------
%% @doc pt1 server.
%% @end
%%%-------------------------------------------------------------------

-module(pt1_srv).

-behaviour(gen_server).

%% -compile([{parse_transform, pt1_srv_transform}]).

%% API
-export([start_link/0, send_async/1, send_sync/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_sync(Req) ->
    gen_server:call(?SERVER, Req).

send_async(Req) ->
    gen_server:cast(?SERVER, {Req, self()}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Args) ->
    {ok, Args}.

handle_call(Request, From, State) ->
    Reply = {p1_srv, Request},
    self() ! {From, Reply},
    {noreply, State}.

handle_cast({Request, Pid}, State) ->
    Pid ! {p1_srv, Request},
    {noreply, State}.

handle_info({From, Reply}, State) ->
    gen_server:reply(From, Reply),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
