%%%-------------------------------------------------------------------
%% @doc sofo clients manager.
%% @end
%%%-------------------------------------------------------------------

-module(sofo_clients).

-behaviour(gen_server).

%% API
-export([run/1,
         add/1,
         remove/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).
-define(CHARS, "qwertyQWERTY1234567890").

-record(state, {clients, port}).

%%====================================================================
%% API functions
%%====================================================================

run([Clients, Port] = Opts) ->
    application:ensure_all_started(lager),
    Args = [list_to_integer(X) || X <- Opts],
    gen_server:start({local, ?SERVER}, ?MODULE, Args, []).

add(Clients) ->
    gen_server:cast(?SERVER, {add, Clients}).

remove(Clients) ->
    gen_server:cast(?SERVER, {remove, Clients}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Clients, Port]) ->
    process_flag(trap_exit, true),
    Pids = spawn_clients(Clients, Port),
    lager:info("Spawned ~p clients", [Clients]),
    {ok, #state{clients = Pids, port = Port}, 5000}.

handle_call(_Req, _From, State) ->
    {reply, {error, unimplemented}, State}.

handle_cast({add, Clients},
            #state{clients = Pids, port = Port} = State) ->
    NewPids = spawn_clients(Clients, Port),
    lager:info("Added ~p clients", [Clients]),
    {noreply, State#state{clients = Pids ++ NewPids}};
handle_cast({remove, Clients}, #state{clients = Pids} = State) ->
    [kill_client(P)
     || {_, P} <- lists:unzip(
                    lists:sublist(
                      lists:sort(
                        [{random:uniform(), P} || P <- Pids]),
                      Clients))],
    lager:info("Removed ~p clients", [Clients]),
    {noreply, State};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, remove_client},
            #state{clients = Pids} = State) ->
    {noreply, State#state{clients = Pids -- [Pid]}};
handle_info({'EXIT', Pid, Reason},
            #state{clients = Pids} = State) ->
    lager:warning("Client exited beacuse ~p",[Reason]),
    {noreply, State#state{clients = Pids -- [Pid]}};
handle_info(timeout, State) ->
    {Cnt, NonResponsive} = lists:foldl(
                             fun(P, {CntAcc, NrAcc}) ->
                                     Ref = make_ref(),
                                     P ! {get_stats, Ref, self()},
                                     receive
                                         {stats, Ref, Cnt} ->
                                             {CntAcc + Cnt, NrAcc}
                                     after
                                         5000 ->
                                             {CntAcc, NrAcc+1}
                                     end
                             end, {0,0}, State#state.clients),
    lager:info("~p processes has sent ~p requests in total",
               [length(State#state.clients), Cnt]),
    lager:info("~p unresponsive processes out of ~p",
               [NonResponsive, length(State#state.clients)]),
    {noreply, State, 5000};
handle_info(_Req, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

spawn_clients(Clients, Port) ->
    [begin
         Pid = spawn_client(Port),
         timer:sleep(100),
         Pid
     end || _ <- lists:seq(1, Clients)].

spawn_client(Port) ->
    spawn_link(
      fun() ->
              {ok, Socket} = gen_tcp:connect({127,0,0,1},
                                             Port,
                                             [{active, false}, binary],
                                             5000),
              client_loop(Socket, 0)
      end).

client_loop(Socket, Cnt) ->
    receive
        {get_stats, Ref, Pid} ->
            Pid ! {stats, Ref, Cnt}
    after
        0 ->
            ok
    end,
    Rand = fun() -> integer_to_list(random:uniform(100)) end,
    N = random:uniform(3),
    [ok = gen_tcp:send(Socket, Rand()) || _ <- lists:seq(1, N)],
                      %% get_random_string(random:uniform(100), ?CHARS)),
    %% [{ok, _} = gen_tcp:recv(Socket, 0) || _ <- lists:seq(1, N)],
    timer:sleep(random:uniform(1000)),
    client_loop(Socket, Cnt+1).


kill_client(Pid) ->
    exit(Pid, remove_client).


get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).
