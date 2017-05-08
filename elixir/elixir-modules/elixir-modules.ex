ExUnit.start()

defmodule ExModules do
  use ExUnit.Case

  test "By default all the Kernel module functions are auto-imported" do
    assert hd(Keyword.get_values(__ENV__.functions, Kernel)) ==
    (Kernel.__info__ :functions)
    
  end

  test "loading" do
    
  end
  
end
