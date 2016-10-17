%%%-------------------------------------------------------------------
%% @doc tasks server.
%% @end
%%%-------------------------------------------------------------------

-module(tasks_srv).

-behaviour(gen_server).

%% API
-export([start_link/0, push/1, pop/0]).

%% Supervisor callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("tasks.hrl").
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, Pid} |
                      {error, {already_started, Pid}} |
                      {error, Reason} when
      Reason :: term(),
      Pid :: pid().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec push(tasks:fun_task()) -> ok.
push(Task) ->
    gen_server:cast(?MODULE, {push, Task}).

-spec pop() -> {ok, tasks:fun_task()} | {error, no_tasks}.
pop() ->
    gen_server:call(?MODULE, pop).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init(any()) -> {ok, tasks:fifo(fun_task())}.
init(_) ->
    {ok, tasks:new()}.

%%====================================================================
%% Internal functions
%%====================================================================

handle_call(pop, _From, Fifo) ->
     case tasks:pop(Fifo) of
         no_tasks ->
             {reply, {error, no_tasks}, Fifo};
         {Task, NewFifo} ->
             log(pop, Fifo),
             {reply, Task, NewFifo}
     end.

handle_cast({push, Task}, Fifo) ->
    log(push, Fifo),
    {noreply, tasks:push(Fifo, Task)}.

%% THIS IS FINE FOR DIALZYER
%% handle_info(print_completed, {fifo, _In, Out} = State) ->
%%     io:format("Completed tasks: ~p~n", [Out]),
%%     {noreply, State};

%% THIS IS FINE FOR DIALZYER TOO
%% -spec handle_info(print_completed, tasks:fifo(fun_task())) ->
%%                          {noreply, tasks:fifo(fun_task())}.
handle_info(print_completed, {fifo, _In, Out} = State) ->
    io:format("Completed tasks: ~p~n", [Out]),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, _, State) ->
    {ok, State}.

%% DIALZYER IS NOT FINE WITH THAT
%% log(Op, {fifo, In, Out}) ->
%%     io:format("===> Op: ~p~nIn: ~p~nOut: ~p~n", [Op,In,Out]).
%% OR
%% -spec log(pop | push, tasks:fifo(fun_task())) -> ok.
%% log(Op, {fifo, In, Out}) ->
%%     io:format("===> Op: ~p~nIn: ~p~nOut: ~p~n", [Op,In,Out]).
%% And generates the warning:
%% The call tasks_srv:log('pop', Fifo::tasks:fifo(fun(() -> any()))) 
%% contains an opaque term as 2nd argument when terms of different 
%% types are expected in these positions
%%
%% Function handle_cast/2 has no local return
%%
%% The call tasks:push(Fifo::{'fifo',_,_}, Task::any()) does not have
%% an opaque term of type tasks:fifo(fun(() -> any())) as 1st argument
%%
%% Invalid type specification for function tasks_srv:log/2. 
%% The success typing is ('pop' | 'push',{'fifo',_,_}) -> 'ok'

log(Op, Fifo) ->
    io:format("===> Op: ~p~nFifo: ~p~n", [Op,Fifo]).
