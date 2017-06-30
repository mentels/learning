defmodule PhxGraphql.Accounts.UserResolver do
  alias PhxGraphql.{Accounts.User, Repo}

  def all(_args, _info) do
    {:ok, Repo.all(User)}
  end

end
