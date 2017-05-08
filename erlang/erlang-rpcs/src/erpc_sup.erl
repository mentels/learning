%%%-------------------------------------------------------------------
%% @doc erpc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erpc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Child1 = {erpc_srv,
              {erpc_srv, start_link, []},
              permanent,
              brutal_kill,
              worker,
              [erpc_srv]},
    Child2 = {erpc_task_sup,
              {erpc_task_sup, start_link, []},
              permanent,
              5000,
              supervisor,
              [erpc_task_sup]},
    {ok, { {one_for_all, 10, 1}, [Child1, Child2]} }.

%%====================================================================
%% Internal functions
%%====================================================================
