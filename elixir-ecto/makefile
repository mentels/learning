.PHONY: postgres

db_container = ecto

postgres:
	docker start $(db_container) || docker run --name $(db_container) -e POSTGRES_PASSWORD=postgres -p 5432:5432 -d postgres
