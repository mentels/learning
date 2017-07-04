defmodule M do

  def empty?(nil) do
    true 
  end
  def empty?(n) when is_number(n) do
    n == 0
  end
  def empty?([])  do
    true
  end
  def empty?(m) when map_size(m) == 0  do 
    true 
  end
  def empty?(_), do: false

end

defmodule User do
  defstruct [:name, :age]
end

defprotocol Empty do
  @fallback_to_any true
  def empty?(t)
end

defimpl Empty, for: Atom do
    def empty?(a), do: a == nil
end
defimpl Empty, for: Integer do
  def empty?(i), do: i == 0
end
defimpl Empty, for: Float do
    def empty?(f), do: f == 0.0
end
defimpl Empty, for: Map do
  def empty?(m), do: map_size(m) == 0
end
defimpl Empty, for: List do
  def empty?(l), do: l == []
end
defimpl Empty, for: User do
  def empty?(u) do
    u.name == nil and u.age == nil
  end
end
defimpl Empty, for: Any do

  defmacro __deriving__(module, struct, opts) do
    if opts[:special] do
      quote do
        defimpl Empty, for: unquote(module) do
          def empty?(struct) do
            IO.puts "helllo"
            struct
            |> Map.from_struct
            |> Map.values
            |> Enum.all?(&(&1 == nil))
          end
        end
      end
    end
  end

  def empty?(u) when is_map(u), do: u.name == nil and u.age == nil 
  def empty?(_), do: false
end

defmodule User1 do
  @derive [Empty]

  def to_string(u) do
    "#{u.name} from #{u.country} (#{u.age})"
  end

  defstruct [:name,
             :age,
             :country]
end

defmodule User2 do
  def to_string(u) do
    "#{u.name} from #{u.surname} (#{u.age})"
  end

  @derive [{Empty, special: true}]
  defstruct [:name, :age, :surname]
end

defmodule M
  do
  def ela(x)
    do
    IO.puts "ela"
  end
end
