ExUnit.start(trace: true)

defmodule TuplesTest do
  @moduledoc """
  Tests for the Collectable and Enumerable implemantations for tuples
  """
  use ExUnit.Case

 test "test Enumerable.count/1" do
    assert Enumerable.count({1,2,3}) === {:ok, 3}
 end

 test "test Enumerable.elem/2" do
   assert Enumerable.member?({1,2,3}, Enum.random(1..3)) === {:ok, true}
   refute Enumerable.member?({1,2,3}, 4) === {:ok, false}
 end

 test "test Enumerable.reduce/3" do
   assert Enumerable.reduce({1,2,3}, {:cont, 0}, &{:cont, &1+&2})
 end

 test "test Collectable.into/1 for lists" do
   original = [1,2,3]
   to_insert = [:a,:b]
   {init_acc, fun} = Collectable.into(original)  
   new =
     to_insert
     |> Enum.reduce(init_acc, fn x, acc -> fun.(acc, {:cont, x}) end) 
     |> fun.(:done)
   assert original ++ to_insert == new
 end

 test "test Collectable.into/1 for tuples" do
   original = {1,2,3}
   to_insert = {:a,:b}
   {init_acc, fun} = Collectable.into(original)  
   new =
     to_insert
     |> Enum.reduce(init_acc, fn x, acc -> fun.(acc, {:cont, x}) end) 
     |> fun.(:done)
   assert {1,2,3,:a,:b} == new
 end

end
