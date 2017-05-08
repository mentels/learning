defmodule Hygiene do
  
  defmacro scopes(val) do
    local = "some value"
    inject = quote do
      local = unquote(val)
      IO.puts "At the end of macro body: local = #{local}"
    end
    IO.puts "In macro definition: local = #{local}"
    inject
  end

  defmacro change_caller_context(val) do
    quote do: var!(dad) = unquote(val)
  end

  defmacro read_caller_context() do
    quote do
      if var!(wife) == "ali", do: :lucky, else: :unlucky
    end
  end
end

defmodule TestHelper do
  ExUnit.start
end

defmodule HygieneTest1 do
  require Hygiene
  local = :ala
  Hygiene.scopes("bootcamp")
  Hygiene.scopes("bootcamp")
  IO.puts "At the end: local = #{local}"
end

defmodule HygieneTest2 do
  require TestHelper
  use ExUnit.Case
  
  defmodule MyMod do
    require Hygiene
    def fun1() do
      dad = "andy"
      Hygiene.change_caller_context("andrew")
      dad
    end

    def fun2() do
      wife = "ali"
      Hygiene.read_caller_context()
    end
  end
    
  test "var! changes caller context" do
    assert MyMod.fun1 == "andrew"
  end

  test "var! read caller context" do
    assert MyMod.fun2 == :lucky
  end
  
end
