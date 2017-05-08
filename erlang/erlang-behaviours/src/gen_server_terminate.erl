%%%-------------------------------------------------------------------
%% @doc sofo connection acceptor.
%% @end
%%%-------------------------------------------------------------------

-module(gen_server_terminate).

-behaviour(gen_server).

%% Test API
-export([not_terminate_on_init_stop_normal/0,
         not_terminate_on_init_stop_abnormal/0,
         not_terminate_on_internal_error/0]).

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

-record(state, {on_terminate}).

%%====================================================================
%% Test functions (no traping exits)
%%====================================================================

%% TODO: Run all fucntion pringint the config

not_terminate_on_init_stop_abnormal() ->
    process_flag(trap_exit, true),
    ?MODULE:start_link(fun(_) -> {stop, abnormal} end),
    receive {'EXIT', _R} = Exit -> flush([Exit]) end.

not_terminate_on_init_stop_normal() ->
    process_flag(trap_exit, true),
    ?MODULE:start_link(fun(_) -> {stop, normal} end),
    receive {'EXIT', _R} = Exit -> flush([Exit]) end.

not_terminate_on_internal_error() ->
    process_flag(trap_exit, true),
    {ok, Pid} = ?MODULE:start_link(on_init()),
    case catch call(Pid, fun() -> error(internal_error) end) of
        {'EXIT', _R} = Exit -> flush([{catched, Exit}])
    end.

on_init() ->
    Pid = self(),
    OnTerminate = fun(Reason) -> Pid ! {terminate_called, Reason} end,
    fun(State) -> {ok, State#state{on_terminate = OnTerminate}} end.

flush(Acc) ->
    receive X -> flush(Acc ++ [X]) after 1000 -> Acc end.

%%====================================================================
%% API functions
%%====================================================================

start_link(OnInitFn) ->
    Args = [OnInitFn],
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

call(Server, Fn) ->
    gen_server:call(Server, Fn).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([OnInitFn]) ->
    OnInitFn(#state{}).

handle_call(Fn, _From, State) ->
    Fn(),
    {reply, ok, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Req, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    (State#state.on_terminate)(Reason).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
