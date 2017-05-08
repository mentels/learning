defimpl String.Chars, for: Map do
  def to_string(map) do
    internal = fn ->
      Enum.reduce(map, nil, fn({k,v}, nil) -> "  #{k} => #{v}"
        ({k,v}, acc) -> "  #{k} => #{v}\n" <> acc end)
    end
    """
    {
    #{internal.()}
    }
    """
  end
end

