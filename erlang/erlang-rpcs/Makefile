.PHONY: compile server client

cnt ?= 100
port ?= 5566

compile:
	rebar3 compile

server: compile
	erl -pa _build/default/lib/*/ebin \
	-sname srv -eval "application:ensure_all_started(erpc)"

client: compile
	erl -pa _build/default/lib/*/ebin -sname client
