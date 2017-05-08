-module(types_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() -> [check_correct_specs, check_bad_specs, check_no_specs].

init_per_suite(Config) ->
    Cwd = element(2, file:get_cwd()),
    build_base_plt(filename:join([Cwd, "../.."])),
    build_app_plt(filename:join([Cwd, "../.."])),
    Config.

check_correct_specs(_Config) ->
    Cwd = element(2, file:get_cwd()),
    dialyze(filename:join([Cwd, "../.."]), correct_specs).

check_bad_specs(_Config) ->
    Cwd = element(2, file:get_cwd()),
    dialyze(filename:join([Cwd, "../.."]), bad_specs).

check_no_specs(_Config) ->
    Cwd = element(2, file:get_cwd()),
    dialyze(filename:join([Cwd, "../.."]), no_specs).


%% Internal

build_base_plt(Cwd) ->
    Plt = filename:join([Cwd, "base.plt"]),
    Opts = [
            {output_file, Plt ++ ".log"},
            {get_warnings, true},
            {apps, [kernel, stdlib]}
            | case filelib:is_regular(Plt) of
                  false ->
                      [{analysis_type, plt_build},
                       {output_plt, Plt}];
                  true ->
                      [{analysis_type, plt_check}]
              end
           ],
    dialyzer:run(Opts).

build_app_plt(Cwd) ->
    Plt = filename:join([Cwd, "app.plt"]),
    Opts = [
            {output_file, Plt ++ ".log"},
            {get_warnings, true},
            {files_rec, [Cwd]}
            | case filelib:is_regular(Plt) of
                  false ->
                      [{analysis_type, plt_build},
                       {output_plt, Plt}];
                  true ->
                      [{analysis_type, plt_check}]
              end
           ],
    dialyzer:run(Opts).

dialyze(Cwd, Module) ->
    Opts = [{output_file, filename:join([Cwd, "check.log"])},
            {analysis_type, succ_typings},
            {plts, [filename:join([Cwd, P]) || P <- ["base.plt", 
                                                     "app.plt"]]},
            {files, [filename:join([Cwd, 
                                    atom_to_list(Module) ++ ".beam"])]},
            {check_plt, false},
            {get_warnings, true}],
    [ct:pal("~s~n", [dialyzer:format_warning(W)]) 
     || W <- dialyzer:run(Opts)].


types_f() ->
    filename:join([element(2,file:get_cwd()), "types.beam"]).

types_f(Cwd) ->
    filename:join([Cwd, "types.beam"]).
