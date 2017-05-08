%%%-------------------------------------------------------------------
%% @doc sofo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sofo_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Port, AllowedReqs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port, AllowedReqs]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Port, AllowedReqs]) ->
    SupFlags = #{strategy => rest_for_one,
                 intensity => 1,
                 perio => 10},
    AcceptorChild = #{id => sofo_acceptor,
                      start => {sofo_acceptor, start_link, [Port]},
                      restart => permanent,
                      shutdown => 5000,
                      type => worker,
                      modules => []},
    ConnSupChild = #{id => sofo_conn_sup,
                     start => {sofo_conn_sup, start_link, [AllowedReqs]},
                     restart => permanent,
                     shutdown => 5000,
                     type => supervisor,
                     modules => []},
    {ok, {SupFlags, [AcceptorChild, ConnSupChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
