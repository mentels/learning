ExUnit.start

defmodule DynCalls do
  use ExUnit.Case

  test "apply calls calls fun.mod with an arity of length(args)" do
    {mod, fun} = {Enum, :map}
    assert apply(Enum, :map, [1..10, &(&1*2)]) == Enum.map(1..10, &(&1*2))
    assert apply(mod, fun, [1..10, &(&1*2)]) == Enum.map(1..10, &(&1*2))
  end

  test "spawn/3 call fun.mod with an arity of length(args)" do
    assert is_pid spawn(Enum, :map, [1..10, &(&1*2)])
    refute_receive _, 500
  end

  test "in mod.fun call fun cannot be a variable" do
    result = Enum.map(1..10, &(&1*2))
    {mod, fun} = {Enum, :map}
    assert mod.map(1..10, &(&1*2)) == result
    assert_raise UndefinedFunctionError,
      fn -> mod.fun(1..10, &(&1*2)) end
  end

  test "module/function names can be evaluated from an expression" do
    result = Enum.map(1..10, &(&1*2))
    mod = fn -> Enum end
    fun = fn -> :map end
    assert mod.().map(1..10, &(&1*2)) == result
    assert (mod.()).map(1..10, &(&1*2)) == result
  end
end
