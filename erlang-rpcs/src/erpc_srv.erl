%%%-------------------------------------------------------------------
%% @doc sofo connection acceptor.
%% @end
%%%-------------------------------------------------------------------

-module(erpc_srv).

-behaviour(gen_server).

%% API
-export([start_link/0,
         timing_out_call/0,
         supervised_timing_out_call/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

timing_out_call() ->
    gen_server:call(?SERVER, {timing_out_call, self()},1000).

supervised_timing_out_call() ->
    erpc_task_sup:add_task(fun() -> ?MODULE:timing_out_call() end).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, undefined}.

handle_call({timing_out_call, Caller}, _From, _State) ->
    link(Caller),
    lager:info("~p ~p: processing request from caller: ~p", [?MODULE,
                                                             self(),
                                                             Caller]),
    timer:sleep(timer:seconds(10)),
    lager:info("~p ~p: request processed the caller is ~s",
               [?MODULE, self(), case is_process_alive(Caller) of
                                     true -> "alive";
                                     false -> "dead"
                                 end]),
    {reply, ok, Caller};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({'EXIT', Caller, Reason}, LastCaller) ->
    case Caller =:= LastCaller of
        true ->
            lager:info("The last calller died because ~p", [Reason]);
        false ->
            lager:info("Some caller died becasue ~p", [Reason])
    end,
    {noreply, undefined};
handle_info(Req, State) ->
    lager:info("Got ~p", [Req]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
