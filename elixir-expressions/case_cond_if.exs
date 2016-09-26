defmodule CaseCondIf2 do
  defmacro my_macro x do
    quote do
      :erlang.is_float(unquote(x))
    end
  end
end

defmodule CaseCondIf do

  def if1 do
    if true, do: :reached
  end

  def if2 do
    if my_fun(:ala), do: :reached
  end

  def if3 do
    if my_fun(nil), do: :wont_reach_not_truthy
  end

  def if4 do
    if false do
      :wont_reach_not_truthy
    else
      :reached
    end
  end

  def if5 do
    if nil do
      :wont_reach_not_truthy
    else
      :reached
    end
  end

  def case1(value) do
    require Bitwise
    require CaseCondIf2
    
    case value do

      v when hd(v) in [1,2,3] or hd(v) in [:a,:b,:c] ->
        IO.puts "v when hd(v) in [1,2,3] or hd(v) in [:a,:b,:c]"
        
      v when is_list(v) -> IO.puts "v when is_list(#{inspect v})"

      ## The below line wont compile
      # v when my_fun(v) -> IO.puts "v when my_fun(#{inspect v})"

      ## The below line wont compile - cannot invoke local macro
      # v when my_macro(v) -> IO.puts "v when my_macro(#{inspect v})"

      ## The below line wont compile - unknown variable x
      # v when CaseCondIf.my_macro(v) -> IO.puts "v when my_macro(#{inspect v})"

      v when CaseCondIf2.my_macro(v) -> IO.puts "v when CaseCondIf2.my_macro(#{inspect v})"

      v when is_integer(v) and Bitwise.~~~(v) != 0 ->
        IO.puts "v when is_integer(#{inspect v}), Bitwise.bnot #{inspect v}"

    end
  end

  def case2(value) do
    case value do
      v when v in [:a,:b,:c] -> IO.puts "v in a list"
      v when v in 1..3 -> IO.puts "vin a range"
      # The below won't compile: ',' and ';' cannot be used to separate guards
      # v when is_float(v), v < 10 -> IO.puts "is <10 and float"
      # v when is_float(v); v < 10 -> IO.puts "is <10 or float"

      v when is_float(v) and v < 10 -> IO.puts "is <10 and float"
      v when is_float(v) or v < 10 -> IO.puts "is <10 or float"
    end
  end

  def valid_age(age) when (age >= 18 and age <= 99) or age == :allowed do
    true
  end
  def valid_age(_) do
    false
  end

  def cond1 s do
    cond do
      (IO.puts("here!!"); String.length(s) && String.starts_with?(s, "a")) -> true
      String.length(s) -> true
      :bad_news -> false
    end
  end
  # Compiler will complain and fail becasue we cannot have def and
  # defp for one function.
  # defp cond1 s do
  #   IO.puts("Got #{inspect(s)}")
  # end

  def empty_fun do
  end

  
      

  defp my_fun(x), do: x

  defmacro my_macro x do
    quote do
      :erlang.is_integer(unquote(x))
    end
  end
end
