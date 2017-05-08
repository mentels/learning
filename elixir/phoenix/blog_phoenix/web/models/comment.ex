defmodule BlogPhoenix.Comment do
  use BlogPhoenix.Web, :model

  schema "comments" do
    field :name, :string
    field :content, :string
    belongs_to :post, BlogPhoenix.Post

    timestamps()
  end

  @required_fields ~w(name content post_id)a
  @optional_fields ~w()a

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, @required_fields)
    |> validate_required(@required_fields)
  end
end
