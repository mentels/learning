defmodule Loop do
  defmacro while(expression, do: block) do
    quote do
      try do
        for _ <- Stream.cycle([:ok]) do
          if unquote(expression) do
            unquote(block)
          else
            Loop.break()
          end
        end
      catch
        :break -> :ok
      end
    end
  end

  def break(), do: throw :break
end

defmodule TestHelper do
  ExUnit.start
end

defmodule UnlessTest do
  require TestHelper
  require Loop
  use ExUnit.Case
  
  test "while/1 loops until the epxression is falsy" do
    # GIVEN
    Process.flag(:trap_exit, true)
    other_pid = spawn(fn -> receive do _ -> :ok end end)
    while_pid = spawn_link(fn -> Loop.while Process.alive?(other_pid) do
        receive do
          {from, :ping} -> send from, :pong
          :stop         -> Loop.break
        after 0 -> :ok
        end
      end
    end)

    # WHEN
    for _ <- random_range(10) do
      send while_pid, {self(), :ping}
      assert_receive :pong
    end
    Process.exit(other_pid, :kill)

    # THEN
    assert_receive {:EXIT, ^while_pid, :normal}, 500
    refute Process.alive?(other_pid)
  end

  test "while/1 loops until the Loop.break is called" do
    # GIVEN
    Process.flag(:trap_exit, true)
    while_pid = spawn_link(fn -> Loop.while true do
        receive do
          {from, :ping} -> send from, :pong
          :stop         -> Loop.break
        after 0 -> :ok
        end
      end
    end)

    # WHEN
    for _ <- random_range(10) do
      send while_pid, {self(), :ping}
      assert_receive :pong
    end
    send while_pid, :stop

    # THEN
    assert_receive {:EXIT, ^while_pid, :normal}, 500
  end

  test "while/1 cannot relay on an expression with variables" do
    # GIVEN
    Process.flag(:trap_exit, true)
    x = 42
    while_pid = spawn_link(fn -> Loop.while x == 42 do
        receive do
          {from, :ping} -> send from, :pong
          :stop         -> x = 24
        after 0 -> :ok
        end
      end
    end)

    # WHEN
    for _ <- random_range(10) do
      send while_pid, {self(), :ping}
      assert_receive :pong
    end
    send while_pid, :stop

    # THEN
    refute_receive {:EXIT, ^while_pid, :normal}, 500
    assert Process.alive?(while_pid)
  end

  
  defp random_range(max), do: 1..(Enum.take_random(1..max, 1) |> hd)

end
