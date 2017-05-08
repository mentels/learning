ExUnit.start

defmodule StreamsTest do
  @moduledoc """
  Tests for learning Elixir Streams
  """

  use ExUnit.Case

  ### Stream.map/2

  test "Stram.map/2 creates a structure that can be passed to Enum
        for enumeration" do
    range = 1..10
    mapper = &(&1 * 2)
    stream = Stream.map(range, mapper)
    assert Enum.map(range, mapper) == Enum.map(stream, &(&1))
  end

  test "Streams enumerate once" do
    pid = Process.spawn(fn ->
      collector(%{:enum => [], :stream => []}) end, [:link])
    enum_fn = fn n -> send(pid, {:enum, n}); n end
    stream_fn = fn n -> send(pid, {:stream, n}); n end
    enum_result = 1..3
    |> Enum.map(enum_fn)
    |> Enum.map(&(&1 * 2))
    |> Enum.map(enum_fn)
    stream_result = 1..3
    |> Stream.map(stream_fn)
    |> Stream.map(&(&1 * 2))
    |> Stream.map(stream_fn)
    |> Enum.to_list
    assert enum_result == stream_result
    send pid, {:finish, self}
    {enum_order, stream_order} =
      receive do ordering when ordering != {[], []}  -> ordering
      after 1000 -> exit(:cannot_collect_ordering) end
    assert [1, 2, 3, 2, 4, 6] == enum_order
    assert [1, 2, 2, 4, 3, 6] == stream_order
  end

  def collector(%{:enum => m, :stream => s} = acc) do
    receive do
      {:enum, n} -> collector(%{acc | :enum => [n | m]})
      {:stream, n} -> collector(%{acc | :stream => [n | s]})
      {:finish, pid} -> send pid, {Enum.reverse(m), Enum.reverse(s)}
    end
  end

  ### Stream.cycle/1 and Stream.unfold/2

  test "Streams allow to cycle throught enumerable" do
    assert Enum.take(Stream.cycle([1,2,3]), 4) == [1,2,3,1]
  end

  test "Streams allow to unfold the enumerables" do
    stream = Stream.unfold(10, fn 0 -> nil; n -> {n, n-2} end)
    assert Enum.take(stream, 3) == [10, 8, 6]
    assert Enum.take(stream, 5) == [10, 8, 6, 4, 2]
    assert Enum.take(stream, 100) == [10, 8, 6, 4, 2]
  end

  ### Stream.chunk/4

  test "Stream.chunk/4 cuts enumerable into pieces" do
    items_cnt = 2
    chunks = Stream.chunk(1..6, items_cnt) |> Enum.to_list
    assert [[1,2],[3,4],[5,6]] == chunks
  end

  test "Stream.chunk/4 cuts enumerable into pieces that overlap" do
    items_cnt = 2
    step = 1
    chunks = Stream.chunk(1..6, items_cnt, step) |> Enum.to_list
    assert [[1,2],[2,3],[3,4],[4,5],[5,6]] == chunks
  end

  test "Stream.chunk/4 cuts enumerable into overlaping pieces" do
    items_cnt = 2
    step = 1
    chunks = Stream.chunk(1..6, items_cnt, step) |> Enum.to_list
    assert [[1,2],[2,3],[3,4],[4,5],[5,6]] == chunks
  end

  test "Stream.chunk/4 cuts enumerable into pieces with gaps" do
    items_cnt = 2
    step = 3
    chunks = Stream.chunk(1..6, items_cnt, step) |> Enum.to_list
    assert [[1,2],[4,5]] == chunks
  end

  test "Stream.chunk/4 cuts enumerable into pieces with gaps and
        padding with less elements" do
    items_cnt = 2
    step = 3
    chunks = Stream.chunk(1..7, items_cnt, step, []) |> Enum.to_list
    assert [[1,2],[4,5],[7]] == chunks
  end

  test "Stream.chunk/4 cuts enumerable into pieces with gaps and
  padding with proper number of elements" do
    items_cnt = 2
    step = 3
    chunks = Stream.chunk(1..7, items_cnt, step, [:x, :y, :z])
    |> Enum.to_list
    assert [[1,2],[4,5],[7,:x]] == chunks
  end
  
end
