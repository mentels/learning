defmodule PhxGraphql.Schema.Types do
  use Absinthe.Schema.Notation
  use Absinthe.Ecto, repo: PhxGraphql.Repo

  object :accounts_user do
    field :id, :id
    field :name, :string
    field :email, :string
    # the :blog_post is an absinthe object while the :blog_posts is the table
    # name
    field :posts, list_of(:blog_post), resolve: assoc(:blog_posts)
  end

  object :blog_post do
    field :id, :id
    field :title, :string
    field :body, :string
    # the :accounts_user is the object defined above while the :accounts_users
    # is the table name
    field :user, :accounts_user, resolve: assoc(:accounts_users)
  end

end
