%%%-------------------------------------------------------------------
%% @doc tasks API.
%% @end
%%%-------------------------------------------------------------------

-module(tasks).

-include("tasks.hrl").
-type ft_fifo() :: fifo(fun_task()).

-export([new/0, push/2, pop/1]).

-spec new() -> ft_fifo().
new() ->
    {fifo, [], []}.

%% THIS WORKS AS WELL
%% -spec push(fifo(A), A) -> fifo(A).
%% push({fifo, In, Out}, X) -> 
%%     {fifo, [X|In], Out}.


-spec push(ft_fifo(), fun_task()) -> ft_fifo().
push({fifo, In, Out}, Task) ->
    {fifo, [Task | In], Out}.

%% -spec pop(ft_fifo()) -> {fun_task(), ft_fifo()};
%%          (ft_fifo()) -> empty.
%% Generates: 
%% Overloaded contract for tasks:pop/1 has overlapping domains; 
%% such contracts are currently unsupported and are simply ignored

-spec pop(ft_fifo()) -> {fun_task(), ft_fifo()} | no_tasks.
pop({fifo, [Task | In], Out}) ->
    {Task, {fifo, In, [Task | Out]}};
pop({fifo, [], _}) ->
    no_tasks.

