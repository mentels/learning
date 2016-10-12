-module(pt1_srv_transform).

-export([parse_transform/2]).

parse_transform(Forms0, _Opts) ->
    Forms1 = transform_bangs_into_gproc_sends(Forms0),
    record_gproc_sends_in_file(Forms1),
    %% TODO: add the gporc_calls fun
    Forms1.
            

%% ! to gproc:send transform

transform_bangs_into_gproc_sends(Forms) ->
    parse_trans:plain_transform(fun bang_to_gproc_send_trans_fn/1, 
                                Forms).

bang_to_gproc_send_trans_fn({op, LINE, '!', LArg, RArg}) ->
    [NewLArg] = parse_trans:plain_transform(
                  fun bang_to_gproc_send_trans_fn/1,
                  [LArg]),
    Func = {remote, LINE, {atom, LINE, gproc}, {atom, LINE, send}},
    {call, LINE, Func, [NewLArg, RArg]};
bang_to_gproc_send_trans_fn(_Form) ->
    continue.

%% finding the gproc:send calls

record_gproc_sends_in_file(Forms0) ->
    CallMFAs = parse_trans:inspect(fun find_gproc_send_calls/4,
                                   _Acc = [],
                                   Forms0,
                                   _Options = []),
    ok = file:write_file("gpcalls", 
                         io_lib:format("~s~n", [CallMFAs]), [write]).

find_gproc_send_calls(_Type, {call, L,
                       {remote, L, {atom, L, gproc}, {atom, L, send}},
                       _Args} = _Form, Context, Acc) ->
    {true,
     begin
         MFA = [parse_trans:context(X, Context) || X <- [module,
                                                          function,
                                                          arity]],
         [format_call(MFA, L) | Acc]
     end};
find_gproc_send_calls(_, _, _, Acc) ->
    {true, Acc}.

format_call(MFA, Line) ->
    io_lib:format("LINE: ~p CALL: ~p~n", [Line, list_to_tuple(MFA)]).

%% other

create_gproc_calls_fun_body(GprocCallMFAs) ->
    ErlAST = erl_syntax:abstract(GprocCallMFAs),
    ErlParse = erl_syntax:revert(ErlAST),
    ErlParse.

create_gproc_calls_fun(ErlParseBody) ->
    L = erl_anno:new(0),
    [{function, L, gproc_calls, 0,
      [{clause, L, _PatternSeq = [], _GuardSeq = [], ErlParseBody}]
     }].














