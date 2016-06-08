erpc
=====

An OTP application

Build
-----

$ rebar3 compile

### About ###

Shows that rpc:cast/3 creates unspervised process that is gone when an error occurs. In this particular example invoking erpc_client:cast_server/1 results in a request to the erpc_srv on the other Erlang node that times out and this error is not captured.

This are different in case of the supervised tasks. An example is shown in the cast_server_supervised.

### Run ###

$ make servev

And observer the logs.

$ make client
```erlang
(client@szm-mac)8> erpc_client:cast_server_supervied('srv@szm-mac').
true
(client@szm-mac)9> erpc_client:cast_server_supervied('srv@szm-mac').
true
```
