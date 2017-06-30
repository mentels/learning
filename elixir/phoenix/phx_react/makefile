.PHONY: postgres create_db server iserver

db_container = phx_react_postgres

postgres:
	docker start $(db_container) || docker run --name $(db_container) -e POSTGRES_PASSWORD=postgres -p 5432:5432 -d postgres

create_db:
	mix ecto.create

server: postgres create_db
	mix phoenix.server

iserver: postgres create_db
	iex -S mix phoenix.server
