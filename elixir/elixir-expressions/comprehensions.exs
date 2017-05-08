defmodule Comprehensions do
  def gen_random_bin(length) do
    for x <- 1..length, into: <<>>, do: <<Enum.random(1..255)>>
  end
end
