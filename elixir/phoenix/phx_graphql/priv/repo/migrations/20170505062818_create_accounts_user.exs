defmodule PhxGraphql.Repo.Migrations.CreatePhxGraphql.Accounts.User do
  use Ecto.Migration

  def change do
    create table(:accounts_users) do
      add :name, :string
      add :email, :string

      timestamps()
    end

  end
end
