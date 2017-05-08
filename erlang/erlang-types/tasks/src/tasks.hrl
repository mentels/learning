-opaque fifo(A) :: {fifo, list(A), list(A)}.

-type fun_task() :: fun(() -> any()).

%% -export_type([fifo/1, fun_task/0]).

-export_type([fifo/1]).
