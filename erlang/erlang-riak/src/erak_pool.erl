%%%-------------------------------------------------------------------
%% @doc erak pool.
%% @end
%%%-------------------------------------------------------------------

-module(erak_pool).

%% API
-export([start_link/0]).

-define(POOL_NAME, erak_pool).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    PoolSize = application:get_env(erak, pool_size, 32).

%%====================================================================
%% Internal functions
%%====================================================================

workers_args(PoolSize, RiakNodes) ->
    [
     begin
         N = (It rem length(RiakNodes)) + 1,
         {NodeIp, NodePort} = lists:nth(N, RiakNodes),
         [NodeIp, NodePort]
     end || It <- lists:seq(1, PoolSize)].
