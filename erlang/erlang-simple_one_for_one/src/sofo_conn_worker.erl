%%%-------------------------------------------------------------------
%% @doc sofo connection acceptor.
%% @end
%%%-------------------------------------------------------------------

-module(sofo_conn_worker).

-behaviour(gen_server).

%% API
-export([start_link/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket :: port(), reqs, allowed_reqs}).

%%====================================================================
%% API functions
%%====================================================================

start_link(AllowedReqs, Socket) ->
    gen_server:start_link(?MODULE, [AllowedReqs, Socket], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([AllowedReqs, Socket]) ->
    process_flag(trap_exit,true),
    inet:setopts(Socket, [{active, true}, binary]),
    {ok, #state{socket = Socket, allowed_reqs = AllowedReqs}}.

handle_call(_Req, _From, State) ->
    {reply, {error, unimplemented}, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Request}, #state{socket = Socket, reqs = Reqs, allowed_reqs = AReqs} = State)
  when Reqs < AReqs ->
    handle_request(Request),
    {noreply, State#state{reqs = Reqs + 1}};
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    {noreply, stop, tcp_closed, State};
handle_info({done, _Reply}, #state{socket = Socket} = State) ->
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_request(Request) when is_binary(Request) ->
    Int = binary_to_integer(Request),
    ReplyTo  = self(),
    spawn_link(fun() -> ReplyTo ! {done, fib(Int)} end),
    spawn_link(fun() -> ReplyTo ! {done, concat(Int)} end).

%% L = binary_to_list(Request),
    %% Children = [Pid
    %%             || {_, Pid, _, _} <- supervisor:count_children(sofo_conn_sup)],
    %% case Children -- [self()] of
    %%     [] ->
    %%         list_to_binary(
    %%           lists:sublist(L, random:uniform(length(L))));
    %%     Pids ->
    %%         Cnt = random:uniform(length(Pids)),
    %%         [P ! {request, L}
    %%          || {_, P} <- lists:unzip(
    %%                         lists:sublist(
    %%                           lists:sort(
    %%                             [{random:uniform(), P} || P <- Pids]),
    %%                           Cnt))],
    %%         receive
    %%             {request, OtherL} ->
    %%                 list_to_binary(
    %%                   lists:sublist(OtherL, random:uniform(OtherL))
    %%                   ++ lists:sublist(L, random:uniform(length(L))))
    %%         after
    %%             300 ->
    %%                 list_to_binary(
    %%                   lists:sublist(L, random:uniform(length(L))))
    %%         end
    %% end.
    %% reverse_binary(Request, <<>>).

reverse_binary(<<"\n",Rest/binary>>, Acc) ->
    reverse_binary(Rest, Acc);
reverse_binary(<<"\r",Rest/binary>>, Acc) ->
    reverse_binary(Rest, Acc);
reverse_binary(<<>>, Acc) ->
    <<Acc/binary, "\n">>;
reverse_binary(<<X,Rest/binary>>, Acc) ->
    reverse_binary(Rest, <<X, Acc/binary>>).

fib(0) -> 1;
fib(N) when N > 0 ->
    N * fib(N - 1).

concat(N) ->
    lists:foldl(fun(L,Acc) ->
                        lists:append(Acc,
                                     [X || X <- lists:seq(1, N*100)])
                end, [], random:uniform(100)).
    
