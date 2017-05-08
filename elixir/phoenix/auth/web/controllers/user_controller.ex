defmodule Auth.UserController do
  @moduledoc """
  """

  use Auth.Web, :controller
  alias Auth.User

  def new(conn, _params) do
    render conn, "new.html",
      changeset: User.register_changeset(%User{})
  end

  def create(conn, %{"user" => params}) do
    case User.register_changeset(%User{}, params) |> Repo.insert do
      {:ok, _user} ->
        conn
        |> put_flash(:info, "Registration successfull")
        |> redirect(to: session_path(conn, :new))
      {:error, changeset} ->
        render conn, "new.html", changeset: changeset
    end
  end
  
end
