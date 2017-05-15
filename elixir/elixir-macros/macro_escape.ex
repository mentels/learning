# THIS WON'T COMPILE
# "map" doesn't exist
# defmodule MacroEscape.Normal do
#   map = %{name: "ala", when: 2011}
#   def ala_map() do
#     map
#   end
# end

# THIS WON'T COMPILE
# invalid quoted expression: %{name: "ala", when: 2011}
# defmodule MacroEscape.Without do
#   map = %{name: "ala", when: 2011}
#   def ala_map() do
#     unquote(map)
#   end
# end

defmodule MacroEscape.With do
  map = %{name: "ala", when: 2011}
  def ala_map() do
    unquote(Macro.escape map)
  end
end




