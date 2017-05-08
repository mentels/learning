defmodule BindQuoted do
  defmacro without_bindings(expr) do
    quote do
      {unquote(expr), unquote(expr)}
    end
  end

  defmacro with_bindings(expr) do
    quote bind_quoted: [e: expr] do
      {e, e}
    end
  end
end

defmodule TestHelper do
  ExUnit.start
end

defmodule UnlessTest do
  require TestHelper
  require BindQuoted
  use ExUnit.Case
  
  test "without bidnings will call the functions twice" do
    # GIVEN
    funny_fun = fn -> send self(), :hello end

    # WHEN
    result = BindQuoted.without_bindings(funny_fun.())

    # THEN
    assert_received :hello
    assert_received :hello
    assert {:hello, :hello} = result
  end

  test "with bidnings will call the functions once" do
    # GIVEN
    funny_fun = fn -> send self(), :hello end

    # WHEN
    result = BindQuoted.with_bindings(funny_fun.())

    # THEN
    assert_receive :hello
    refute_receive :hello
    assert {:hello, :hello} = result
  end
  
end
