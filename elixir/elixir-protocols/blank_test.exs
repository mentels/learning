ExUnit.start

defmodule BlankTest do
  @moduledoc """
  Tests for the Blank protocol
  """
  use ExUnit.Case

 test "blank for List" do
    assert Blank.blank?([])
    refute Blank.blank?([:ala])
  end

  test "blank for Integer" do
    refute Blank.blank?(1)
    catch_error Blank.blank?(1.0)
  end

  test "blank for Map" do
    assert Blank.blank?(%{})
    refute Blank.blank?(%{ala: :honey})
  end

  test "blank for Atom" do
    assert Blank.blank?(nil)
    assert Blank.blank?(false)
    refute Blank.blank?(:alice)
  end

  test "blank for struct that implements the protocol" do
    assert Blank.blank?(%User{name: "", age: nil})
    refute Blank.blank?(%User{name: "alice"})
  end

  test "blank for struct that doesn't implement the protocol" do
    catch_error Blank.blank?(%UnimplUser{name: "", age: nil})
  end

  test ("blank is automatically implemented for DerviveUser based
         on  implementation for Any") do
    refute Blank.blank?(%DeriveUser{age: 35})
  end

  test ("NotBlank is automatically implemented for FallbackUser based
        on implementation for Any") do
    assert NotBlank.not_blank?(%FallbackUser{})
  end

end
