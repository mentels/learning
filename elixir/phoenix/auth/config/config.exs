use Mix.Config

config :auth,
  ecto_repos: [Auth.Repo]


config :auth, Auth.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "TC4hCqIzcZJCkN84J1/tQ1mKTd3ZI0p+h5LcXVRIbzJ1YkTXPSPXWxSvQox9X9U0",
  render_errors: [view: Auth.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Auth.PubSub,
           adapter: Phoenix.PubSub.PG2]

config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]


config :guardian, Guardian,
  allowed_algos: ["HS512"], # optional
  verify_module: Guardian.JWT,  # optional
  issuer: "auth",
  ttl: { 30, :days },
  verify_issuer: true, # optional
  secret_key: "C0FtTsadf8Gb/15x0Ls36YGhgt8CxQoKpFhl8L1vUndM0lmhxC7dghCZvlAY31Tj",
  serializer: Auth.GuardianSerializer

import_config "#{Mix.env}.exs"
