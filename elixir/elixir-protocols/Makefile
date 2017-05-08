.PHONY: compile test clean

compile:
	elixirc  *.ex

test: compile
	elixir blank_test.exs
	elixir tuples_test.exs

clean:
	rm *beam
