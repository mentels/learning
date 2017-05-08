defmodule JsonApi.AuthorController do
  use JsonApi.Web, :controller

  def show(conn, %{"name" => name}) do
    author = %{:name => name, :surname => :surname,
               :books => JsonApi.Db.books(name)}
    render conn, "show.json", data: author
  end
end
