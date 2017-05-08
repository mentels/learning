%%%-------------------------------------------------------------------
%% @doc sofo connection acceptor.
%% @end
%%%-------------------------------------------------------------------

-module(sofo_acceptor).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {lsocket :: port()}).

%%====================================================================
%% API functions
%%====================================================================

start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Port]) ->
    {ok, LSocket} = gen_tcp:listen(Port, [{active, false}, {reuseaddr, true}]),
    gen_server:cast(self(), accept),
    lager:info("Accpetor listening on port ~p", [Port]),
    {ok, #state{lsocket = LSocket}}.

handle_call(_Req, _From, State) ->
    {reply, {error, unimplemented}, State}.

handle_cast(accept, State) ->
    {ok, Socket} = gen_tcp:accept(State#state.lsocket),
    ok = sofo_conn_sup:handle_connection(Socket),
    gen_server:cast(self(), accept),
    {noreply, State};
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
