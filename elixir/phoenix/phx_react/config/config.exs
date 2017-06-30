# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :phx_react,
  ecto_repos: [PhxReact.Repo]

# Configures the endpoint
config :phx_react, PhxReact.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "l5NoFbmQ4crhOeEEfMHc4dEtF74PS9L7Bcvtry7uBRgrPLzSMTad7V4+VYzJRnz6",
  render_errors: [view: PhxReact.ErrorView, accepts: ~w(html json)],
  pubsub: [name: PhxReact.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
