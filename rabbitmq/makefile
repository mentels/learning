.PHONY: server client_elixir

server:
	docker run -d --hostname my-rabbit --name some-rabbit -p 8080:15672 rabbitmq:3-management

server_web:
	# guest/gues
	open http://localhost:8080

client_elixir:
	cd elixir && iex -S mix

