ExUnit.start

defmodule HandlingExceptionsTests do
  use ExUnit.Case

  test "Kernel.raise/1 allows to raise a RuntimeError" do
    assert_raise RuntimeError, fn -> raise("my error message") end
  end

  test "Kernel.raise/2 allows to raise a custom error" do
    assert_raise CustomException, fn -> raise(CustomException) end
  end

  test "Try/rescue allows to catch an exception" do
    ex = try do
           raise "haha"
         rescue
           e in RuntimeError -> e
         end
    assert %RuntimeError{message: "haha"} == ex
  end

  test "Try/rescue allows to pattern match on exception type/class" do
    try do
      throw(:reason)
    catch
      :throw, reason ->
        assert :reason == reason
    end
  end

  test "Try/after can be used to soft-guarantee
  releasing the resources" do
    # Soft means that after won't be executed if a linked process dies
    # and the exit signal kills this process.
    {:ok, f} = File.open "/tmp/sample", [:utf8, :write]
    test_fn = fn ->
      try do
        IO.write f, "heloł!"
        raise "Die!"
      after
        File.close(f)
      end
    end
    try do
      test_fn.()
    rescue
      e in RuntimeError ->
        refute Process.alive?(f)
    end
  end

  test "A fun can be automatically wrapped in try by using 'after' just
  right the body" do
    pid = self
    spawn(fn -> TryWrappedFun.send_from_cleanup(pid) end)
    receive do
      {pid, :cleaning_up} ->
        :ok
    after
      500 ->
        flunk "Process #{inspect(pid)} hasn't performed the cleanup!"
    end
  end

  test "Kernel.defexception/1 allows to create sophisticated
  expcetions" do
    {arg1, arg2} = {"helloł", 26}
    try do
      raise(SophisticatedCustomException, arg1: arg1, arg2: arg2)
    rescue
      e ->
        assert e.message() ==  "Exception was raised as " <>
        "raise(#{inspect e.__struct__}, arg1: #{inspect arg1}," <>
        "arg2: #{inspect arg2})"
    end
  end
  
end

defmodule CustomException do
  defexception message: "I'm custom"
end

defmodule SophisticatedCustomException do
  defexception [:message, :arg1, :arg2]

  def exception(kwlist) do
    val1 = kwlist[:arg1]
    val2 = kwlist[:arg2]
    %SophisticatedCustomException{
      message: "Exception was raised as " <>
      "raise(SophisticatedCustomException, arg1: #{inspect val1}," <>
      "arg2: #{inspect val2})"}
  end

end

defmodule TryWrappedFun do
  def send_from_cleanup(pid) do
    raise "bang bang"
  after
    send pid, {self, :cleaning_up}
  end
end
