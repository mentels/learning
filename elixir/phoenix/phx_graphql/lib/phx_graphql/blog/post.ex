defmodule PhxGraphql.Blog.Post do
  use Ecto.Schema

  schema "blog_posts" do
    field :body, :string
    field :title, :string
    belongs_to :accounts_users, PhxGraphql.Accounts.User,
      foreign_key: :accounts_users_id

    timestamps()
  end
end
