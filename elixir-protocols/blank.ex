### Protocol definition

defprotocol Blank do
  @doc "Return true if data is considered blank/empty"
  def blank?(data)
end

defimpl Blank, for: Any do
  def blank?(_), do: false
end

defprotocol NotBlank do
  @doc "Return true if data is considered not blank/empty"
  @fallback_to_any true
  def not_blank?(data)
end

defimpl NotBlank, for: Any do
  def not_blank?(_), do: true
end

### Structs

defmodule User do
  @moduledoc """
  The Blank protocol is explicitly defined for the User{} struct with
  defimpl Blank, for User.
  """
  defstruct name: "Szymon", age: 25
end

defmodule UnimplUser do
  @moduledoc """
  There is no Blank protocol implementation for the UnimplUser{}
  struct. Trying to apply the protocol on it generates an
  Protocol.UndefinedError
  """
  defstruct name: "Szymon", age: 25
end

defmodule DeriveUser do
  @moduledoc """
  The Blank protocol is _not_ explicitly defined for the DeriveUser{}
  struct but its is derived from the protocol implementation for Any
  through the @derive attribute attached to the struct.
  
  The implementation for Any has to be known when compiling this
  module.
  """
  @derive Blank
  defstruct name: "Szymon", age: 25
end

defmodule FallbackUser do
  @moduledoc """
  The NotBlank protocol is _not_ explicitly defined for the FallbUser{}
  struct but its is derived from the protocol implementation for Any
  through the @fallback_to_any attribute attached to the protocol
  implementation.
  
  The implementation for Any has to be known when compiling this
  module.
  """
  defstruct name: "Szymon", age: 25
end

### Protocol Implementation for Specific types

defimpl Blank, for: List do
  def blank?([]), do: true
  def blank?(_), do: false
end

defimpl Blank, for: Integer do
  def blank?(_), do: false
end

defimpl Blank, for: Map do
  def blank?(map), do: map_size(map) == 0
end

defimpl Blank, for: Atom do
  def blank?(atom) when atom == :false or atom == :nil, do: true
  def blank?(_), do: false
end

defimpl Blank, for: User do
  def blank?(%User{name: "", age: nil}), do: true
  def blank?(_), do: false
end

#### Impelemtation of built-in protocols

## TODO
