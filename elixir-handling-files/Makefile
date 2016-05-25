.PHONY: compile test clean

compile:
	-elixirc *.ex

test: compile
	elixir *_test.exs

clean:
	rm *beam
