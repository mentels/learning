.PHONY: compile upgrade clean deep-clean dialyzer shell rel

compile: rebar3
	./rebar3 compile

abstract_compile:
	./rebar3 as abstract compile

abstract_cat:
	cat _build/abstract/lib/pt1/ebin/$(file).pp

parsed_compile:
	./rebar3 as parsed compile

upgrade: rebar3
	./rebar3 upgrade

clean: rebar3
	./rebar3 clean

deep-clean:
	./rebar3 clean -a

dialyzer: rebar3
	./rebar3 dialyzer

shell: compile
	erl -pa _build/default/lib/*/ebin \
	-eval "application:ensure_all_started(pt1)"

rel:
	./rebar3 release
