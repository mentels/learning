defmodule HelloPhoenix.VideoTest do
  use HelloPhoenix.ModelCase

  alias HelloPhoenix.Video

  @valid_attrs %{approved_at: %{day: 17, hour: 14, min: 0, month: 4, sec: 0, year: 2010}, description: "some content", likes: 42, name: "some content", views: 42}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Video.changeset(%Video{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Video.changeset(%Video{}, @invalid_attrs)
    refute changeset.valid?
  end
end
