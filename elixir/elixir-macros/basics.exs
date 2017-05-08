defmodule Macro do
  defmacro mac(expr) do
    IO.puts inspect(expr)
  end
end

defmodule MacroTest do
  require Macro, as: M
  
  M.mac 1
  M.mac  "ala"
  M.mac :atom
  M.mac [1,2,3]
  M.mac {:a,:b}
  M.mac {:a,:b,:c}
  M.mac Enum.map([1,2,3],&(&1*2))

end

  
  
