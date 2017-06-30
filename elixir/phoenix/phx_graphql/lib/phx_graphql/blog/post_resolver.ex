defmodule PhxGraphql.Blog.PostResolver do
  alias PhxGraphql.{Blog.Post, Repo}

  def all(_args, _info) do
    {:ok, Repo.all(Post)}
  end

end
