%%%-------------------------------------------------------------------
%% @doc erpc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erpc_task_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         add_task/1,
         start_task/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_task(Fn) ->
    {ok, _Pid} = supervisor:start_child(?SERVER, [Fn]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Child = {erpc_task,
              {?MODULE, start_task, []},
              transient,
              brutal_kill,
              worker,
              [?MODULE]},
    {ok, { {simple_one_for_one, 10, 1}, [Child]} }.

%%====================================================================
%% Internal functions
%%====================================================================

start_task(Fn) ->
    lager:info("Task started.."),
    {ok, spawn_link(fun() -> Fn() end)}.
