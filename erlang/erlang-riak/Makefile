.PHONY: riak_install

auto_cluster ?= 1
cluster_size ?= 5

all: docker-riak compile

## Riak

docker-riak:
	git clone https://github.com/hectcastro/docker-riak.git

riak_install: docker-riak
	cd docker-riak && make build

riak_run:
	cd docker-riak && \
	DOCKER_RIAK_AUTOMATIC_CLUSTERING=$(auto_cluster) \
	DOCKER_RIAK_CLUSTER_SIZE=$(cluster_size) \
	make start-cluster 

riak_stop_cluster:
	cd docker-riak && make stop-cluster

riak_test_cluster:
	cd docker-riak && make test-cluster

## Erl

shell:
	erl -pa deps/*/ebin -pa ebin/

## Rebar

get-deps:
	./rebar get-deps

compile: get-deps
	./rebar compile



clean:
	./rebar clean -a
