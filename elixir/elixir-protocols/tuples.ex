defmodule Tuples do
  defimpl Enumerable, for: Tuple do

    def count(t) do
      {:ok, tuple_size(t)}
    end

    def member?(t, element) do
      n = tuple_size(t)
      try do
        for i <- 0..n-1 do
          elem(t, i) === element && throw(:found)
        end
      catch
        :throw, :found ->
          {:ok, true}
      end
    end

    def reduce(_, {:halt, acc}, _fun), do: {:halted, acc}
    def reduce(t, {:suspend, acc}, fun), do: {:suspended, acc, &reduce(t, &1, fun)}
    def reduce({}, {:cont, acc}, _fun), do: {:done, acc}
    def reduce(t, {:cont, acc}, fun) do
      rest = tl(:erlang.tuple_to_list(t))
      reduce(:erlang.list_to_tuple(rest), fun.(elem(t, 0), acc), fun)
    end

  end

  defimpl Collectable, for: Tuple do
    def into(original) do
      {[], fn
        acc, {:cont, x} -> [x | acc]
        acc, :done -> List.to_tuple(Tuple.to_list(original) ++ Enum.reverse(acc))
      end}
    end
  end
end
