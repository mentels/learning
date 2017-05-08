* Run: `make test`

* Structs doesn't follow the map protocol - it has to be defined for every struct
  If there's no protocol definition for the `%User{}` we end up with:
  ```elixir
  iex(5)> u = %User{}
  %User{age: 25, name: "Szymon"}
  iex(6)> Blank.blank?(u)
  ** (Protocol.UndefinedError) protocol Blank not implemented for %User{age: 25, name: "Szymon"}
    blank.ex:1: Blank.impl_for!/1
    blank.ex:3: Blank.blank?/1
  ```

* TODOs
  * [ ] Implement example for the built-in protocols
