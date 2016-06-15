-module(erpc_client).

-compile(export_all).

cast_server(Server) ->
    rpc:cast(Server, erpc_srv, timing_out_call, []).

cast_server_supervied(Server) ->
    rpc:cast(Server, erpc_srv, supervised_timing_out_call, []).

cast_server_with_big_data(Server, MBs) ->
    rpc:cast(Server, erpc_srv, regular_call,
             [binary:copy(<<"b">>, MBs * 1000 * 1000)]).

flood_server_with_messages(Server, Kbs, MsgsNo) ->
    [spawn(fun() ->
                  rpc:cast(Server, erpc_srv, regular_call,
                           [binary:copy(<<"b">>, Kbs * 1000)])
           end) || _ <- lists:seq(1, MsgsNo)].
