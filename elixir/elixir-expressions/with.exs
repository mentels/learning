defmodule With do
  def foo(n) do
    with {:ok, x} <- bar(n),
         {:ok, y} <- baz(x) do
      y
    else
      :nan -> IO.puts "foo(#{inspect n}) was called; number required"
      y -> y
    end
  end

  def bar(x) when is_number(x), do: {:ok, x+1}
  def bar(_), do: :nan

  def baz(x) when rem(x,2) == 0, do: {:ok, div(x,2)}
  def baz(_), do: :odd
end
