defmodule PhxPubsub do
  @moduledoc """
  Documentation for PhxPubsub.
  """

  alias Phoenix.PubSub

  @adapter_name :pg2_adapter

  def adapter_name(), do: @adapter_name

  def subcribe_to_topic(topic), do: PubSub.subscribe(@adapter_name, topic)

  def broadcast_on_topic(topic, message),
    do: PubSub.broadcast(@adapter_name, topic, message)

  def broadcast_on_topic_skipping_pid(topic, message, pid),
    do: PubSub.broadcast_from(@adapter_name, pid, topic, message)

end
