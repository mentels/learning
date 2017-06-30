defmodule PhxPubsub.Mixfile do
  use Mix.Project

  def project do
    [
      app: :phx_pubsub,
      version: "0.1.0",
      elixir: "~> 1.5-rc",
      start_permanent: Mix.env == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {PhxPubsub.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:phoenix_pubsub, "~> 1.0"},
    ]
  end
end
