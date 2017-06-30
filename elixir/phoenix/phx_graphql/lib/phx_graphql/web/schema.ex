defmodule PhxGraphql.Schema do
  use Absinthe.Schema
  import_types PhxGraphql.Schema.Types

  query do
    field :blog_posts, list_of(:blog_post) do
      resolve &PhxGraphql.Blog.PostResolver.all/2
    end

    field :accounts_users, list_of(:accounts_user) do
      resolve &PhxGraphql.Accounts.UserResolver.all/2
    end
  end
end
