defmodule MyMacro do
  defmacro unless(expr, clauses) do
    quote do: if !unquote(expr), unquote(clauses)
  end

  defmacro times_n(n) do
    quote do
      def unquote(:"times_#{n}")(x) when is_integer(x) do
        unquote(n)*x
      end
    end
  end

end

defmodule TestHelper do
  ExUnit.start
end

defmodule UnlessTest do
  require TestHelper
  require MyMacro
  use ExUnit.Case
  

  test "unless/2 works for direct expressions" do
    assert MyMacro.unless(false, do: :value) == :value
    assert(MyMacro.unless nil do
            :value
    end == :value)
    refute MyMacro.unless(true, do: :value)
    refute MyMacro.unless(1, do: :value)
  end

  test "unless/2 works for evaluated expression" do
    assert MyMacro.unless(String.to_atom("false"), do: :value) == :value
    assert MyMacro.unless(id(false), do: :value) == :value
    refute MyMacro.unless(id(true), do: :value) == false
  end

  test "unless/2 works with the else: block" do
    assert(MyMacro.unless(false, do: :got_falsy, else: :got_truthy)
      == :got_falsy)
  end

  defp id(x), do: x
  
end

defmodule TimesTest do
  require TestHelper
  use ExUnit.Case

  defmodule Times do
    import MyMacro, only: [times_n: 1]
    times_n(3)
    times_n(4)
  end

  test "times works for numeric argument" do
    assert Times.times_3(3) == 9
    assert Times.times_4(3) == 12
    assert_raise(FunctionClauseError, fn -> Times.times_4("haha") end)
  end
  
end
