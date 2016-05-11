-module(pt1_srv_transform).

-export([parse_transform/2]).

parse_transform(Forms0, _Opts) ->
    Forms1 = parse_trans:plain_transform(fun plain_transform_fn/1,
                                         Forms0),
    GprocCallMFAs = parse_trans:inspect(fun inspect_fn/4,
                                        [],
                                        Forms1,
                                        []),
    file:write_file("gpcalls", io_lib:format("~p~n", [GprocCallMFAs]),
                    [write]),
    %% TODO: add the gporc_calls fun
    Forms1.
            


plain_transform_fn({op, LINE, '!', LArg, RArg}) ->
    [NewLArg] = parse_trans:plain_transform(fun plain_transform_fn/1,
                                            [LArg]),
    Func = {remote, LINE, {atom, LINE, gproc}, {atom, LINE, send}},
    {call, LINE, Func, [NewLArg, RArg]};
plain_transform_fn(_Form) ->
    continue.

inspect_fn(_Type, {call, L,
                       {remote, L, {atom, L, gproc}, {atom, L, send}},
                       _Args} = Form, Context, Acc) ->
    {true,
     begin
         MFAs = [parse_trans:context(X, Context) || X <- [module,
                                                          function,
                                                          arity]],
         [list_to_tuple(MFAs) | Acc]
     end};
inspect_fn(_, _, _, Acc) ->
    {true, Acc}.


create_gproc_calls_fun_body(GprocCallMFAs) ->
    ErlAST = erl_syntax:abstract(GprocCallMFAs),
    ErlParse = erl_syntax:revert(ErlAST),
    ErlParse.

create_gproc_calls_fun(ErlParseBody) ->
    L = erl_anno:new(0),
    [{function, L, gproc_calls, 0,
      [{clause, L, _PatternSeq = [], _GuardSeq = [], ErlParseBody}]
     }].














