.PHONY: postgres create_db server iserver

db_container = phx_chat_postgres

postgres:
	docker start $(db_container) || docker run --name $(db_container) -e POSTGRES_PASSWORD=postgres -p 5432:5432 -d postgres

db_create:
	mix ecto.create

db_migrate:
	mix ecto.migrate

db_connect:
	docker run -it --rm --link $(db_container):postgres postgres psql -h postgres -U postgres


server: postgres create_db
	mix phx.server

iserver: postgres create_db
	iex -S mix phx.server

idistserver: postgres create_db
	PORT=$(port) iex --sname $(name) -S mix phx.server
