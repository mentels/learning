-module(erpc_client).

-compile(export_all).

cast_server(Server) ->
    rpc:cast(Server, erpc_srv, timing_out_call, []).

cast_server_supervied(Server) ->
    rpc:cast(Server, erpc_srv, supervised_timing_out_call, []).
