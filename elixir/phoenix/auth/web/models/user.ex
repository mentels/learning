defmodule Auth.User do
  use Auth.Web, :model
  alias Comeonin.Bcrypt

  schema "users" do
    field :email, :string
    field :password_hash, :string

    field :password, :string, virtual: true
    field :password_confirmation, :string, virtual: true

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def register_changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:email, :password_hash, :password_confirmation])
    |> validate_required(
      [:email, :password_hash, :password_confirmation])
    |> validate_format(:email, ~r/@/)
    |> validate_length(:password, min: 8)
    |> validate_confirmation(:password)
    |> hash_password
    |> unique_constraint(:email)
  end

  defp hash_password(%{valid?: true} = changeset) do
    hashed_pass =
      changeset
      |> get_field(:password)
      |> Bcrypt.hashpwsalt
    put_change(changeset, :password_hash, hashed_pass)
  end
  defp hash_password(changeset), do: changeset
end
