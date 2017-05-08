%%%-------------------------------------------------------------------
%% @doc sofo connection supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sofo_conn_sup).

-behaviour(supervisor).

%% API
-export([start_link/1,
         handle_connection/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(AllowedReqs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [AllowedReqs]).

handle_connection(Socket) ->
    {ok, Pid} = supervisor:start_child(?SERVER, [Socket]),
    ok = gen_tcp:controlling_process(Socket, Pid).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([AllowedReqs]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 60},
    ChildSpec = #{id => sofo_conn_handler,
                  start => {sofo_conn_worker, start_link, [AllowedReqs]},
                  restart => temporary,
                  shutdown => brutal_kill,
                  type => worker,
                  modules => [sofo_conn_worker]},
    {ok, {SupFlags, [ChildSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
