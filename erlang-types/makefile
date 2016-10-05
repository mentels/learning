.PHONY: compile test clean all

cases ?=

all: compile test

compile:
	erlc +debug_info *.erl

test:
	ct_run -logdir ct_log -suite types_SUITE -case $(cases)

clean:
	-rm *.beam
	-rm *.plt
	-rm *.log
	-rm -rf ct_log
