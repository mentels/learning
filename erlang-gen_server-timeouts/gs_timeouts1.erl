-module(gs_timeouts1).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {t1}).

%%%===================================================================
%%% APIp
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% After init/0 the gen_server process infinitely waits for messages
%% But we can set a timeout, that will trigger a 'timeout' message
%% unless there's a message from an other process
init([]) ->
    {ok, #state{t1 = erlang:monotonic_time()}, _Milis = 1000}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, #state{t1 = T1} = State) ->
    Diff = erlang:monotonic_time() - T1,
    io:format("We got timeout after ~p~n",
              [erlang:convert_time_unit(Diff, native, milli_seconds)]),
    {noreply, State#state{t1 = erlang:monotonic_time()}, 1000};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
