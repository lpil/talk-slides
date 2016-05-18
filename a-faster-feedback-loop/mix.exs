defmodule Talk do
  use Mix.Project

  def project do
    [
      app: :talk,
      version: "0.1.0",
      elixir: "~> 1.0",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      name: "talk",
      description: "talk",
    ]
  end

  def application do
    [
      applications: [],
    ]
  end

  defp deps do
    [
      # File system event watcher
      {:fs, "~> 0.9.1"},
    ]
  end
end
