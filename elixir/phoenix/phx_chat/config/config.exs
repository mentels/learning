# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :phx_chat,
  ecto_repos: [PhxChat.Repo]

# Configures the endpoint
config :phx_chat, PhxChat.Web.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "P6E+Eli0rOMFEve0zWE88NTdQrcG6eimqW5Nvb8WjDicM90D/Nx/lMqQJMH6Hfil",
  render_errors: [view: PhxChat.Web.ErrorView, accepts: ~w(html json)],
  pubsub: [name: PhxChat.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
