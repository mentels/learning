%%%-------------------------------------------------------------------
%% @doc tasks top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(tasks_sup).

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
    Child = #{id => tasks_srv, start => {tasks_srv, start_link, []}},
    {ok, { {one_for_all, 0, 1}, [Child]} }.

%%====================================================================
%% Internal functions
%%====================================================================
