defmodule PhxGraphql.Accounts.User do
  use Ecto.Schema

  schema "accounts_users" do
    field :email, :string
    field :name, :string
    has_many :blog_posts, PhxGraphql.Blog.Post, foreign_key: :accounts_users_id

    timestamps()
  end
end
