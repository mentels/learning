defmodule Sup do
  import Supervisor.Spec

  def start do
    children = [
      worker(Worker, [:permanent], [id: :permanent]),
      worker(Worker, [:transient], [id: :transient, restart: :transient])
    ]
    Supervisor.start_link(children, [strategy: :rest_for_one])
  end

end

defmodule Worker do
  def start_link(name), do: {:ok, spawn_link(
                                fn -> IO.puts "Worker #{name} started"
                                  receive do x -> x end
                                end)}
end
