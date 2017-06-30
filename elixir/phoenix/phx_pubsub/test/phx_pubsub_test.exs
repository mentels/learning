defmodule PhxPubsubTest do
  use ExUnit.Case
  doctest PhxPubsub

  @topic "my_topic"

  setup context do
    pid = spawn(fn -> receive do
        {:broadcast, msg} ->
          PhxPubsub.broadcast_on_topic @topic, msg
        {:broadcast_from, msg, excluded_pid} ->
          PhxPubsub.broadcast_on_topic_skipping_pid @topic, msg, excluded_pid
      end end)
    Map.put(context, :pid, pid)
  end

  test "subscribes to topic and receives a notification", %{pid: pid} do
    PhxPubsub.subcribe_to_topic @topic
    send(pid, {:broadcast, msg = {:my_msg, :hello}})
    assert_receive ^msg
  end

  test "they subscribe to a topic and receive notifications", %{pid: pid} do
    # GIVEN
    parent = self()
    subscribers_cnt = 3
    pids = for i <- 1..subscribers_cnt, do: spawn_link(fn ->
          send(parent, {self(), :ready})
          PhxPubsub.subcribe_to_topic @topic
          receive do msg -> send(parent, {self(), msg}) end
        end)
    for p <- pids, do: assert_receive {^p, :ready}

    # WHEN
    send(pid, {:broadcast, msg = :hello})

    # Then
    for i <- 1..subscribers_cnt, do: assert_receive {i, ^msg}
  end

  test "they subscribe to a topic and one doesn't receive notification",
    %{pid: pid} do
    # GIVEN
    parent = self()
    subscribers_cnt = 3
    pids = for i <- 1..subscribers_cnt, do: spawn_link(fn ->
          send(parent, {self(), :ready})
          PhxPubsub.subcribe_to_topic @topic
          receive do msg -> send(parent, {self(), msg}) end
        end)
    [excluded | rest] = pids
    for p <- pids, do: assert_receive {^p, :ready}

    # WHEN
    send(pid, {:broadcast_from, msg = :hello, excluded})

    # Then
    for p <- rest, do: assert_receive {^p, ^msg}
    refute_receive {^excluded, ^msg}
  end


end
