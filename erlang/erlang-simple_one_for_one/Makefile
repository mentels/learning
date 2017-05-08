.PHONY: compile server client

cnt ?= 100
port ?= 5566

compile:
	rebar3 compile

server: compile
	erl -pa _build/default/lib/*/ebin -lager mykey 	\
	-name srv -eval "application:ensure_all_started(sofo)"

clients: compile
	erl -pa _build/default/lib/*/ebin \
	-name clients -run sofo_clients run $(cnt) $(port)
