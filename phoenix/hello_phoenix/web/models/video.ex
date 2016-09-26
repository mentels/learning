defmodule HelloPhoenix.Video do
  use HelloPhoenix.Web, :model

  schema "videos" do
    field :name, :string
    field :approved_at, Ecto.DateTime
    field :description, :string
    field :likes, :integer
    field :views, :integer
    belongs_to :user, HelloPhoenix.User

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:name, :approved_at, :description, :likes, :views])
    |> validate_required([:name, :approved_at, :description, :likes, :views])
  end
end
