.PHONY: postgres create_db server

db_container = chat-postgres

postgres:
	docker start $(db_container) || docker run --name $(db_container) -e POSTGRES_PASSWORD=postgres -p 5432:5432 -d postgres

create_db:
	mix ecto.dump || mix ecto.create

server: postgres create_db
	mix phoenix.server
