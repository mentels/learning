defmodule Dist do

  @doc """
  Registers global process with a `name`. The global process will echo
  messages: {:global, reply_to, msg} -> {:global_echo, msg}
  """
  def register_global(name) do
    :yes = :global.register_name(name, spawn(
          fn ->
            receive do
              {:global, reply_to, msg} -> send reply_to, msg
            end
            spawn fn -> Dist.register_global(name) end
          end))
  end

  def send_to_global(name, msg \\ {:ali})
  # The {NodeName, ProcName} doesn't work
  def send_to_global({_node, _proc_name} = name, msg) do
    send(name, {:global, self, msg})
  end
  def send_to_global(name, msg), do: :global.send(name,
        {:global, self, msg})
    
end
