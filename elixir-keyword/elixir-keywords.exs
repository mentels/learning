ExUnit.start

defmodule KwsTest do
  use ExUnit.Case

  test "kw[:foo] returns the first occurence for a key :foo" do
    kw = [ala: 29, szymon: 26, ala: 31]
    assert kw[:ala] == 29
  end

  test "when matching on keywords all the keys have to be specified;
  in the rigth order" do
    kw = [ala: 29, szymon: 26, ala: 31]
    assert_raise(MatchError, fn -> [ala: a] = kw end)
    assert_raise(MatchError, fn -> [ala: a, szymon: sz] = kw end)
    assert [ala: a1, szymon: sz, ala: a2] = kw
  end
  
end
