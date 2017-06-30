defmodule PhxPubsub.Application do
  @moduledoc false
  import Supervisor.Spec

  use Application

  def start(_type, _args) do
    # List all child processes to be supervised
    children = [
      supervisor(Phoenix.PubSub.PG2, [PhxPubsub.adapter_name, _opts = []])
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: PhxPubsub.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
