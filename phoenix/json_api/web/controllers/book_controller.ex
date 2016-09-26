defmodule JsonApi.BookController do
  use JsonApi.Web, :controller

  def index(conn, %{"name" => author_name}) do
    render conn, "show.json", data: JsonApi.Db.books(author_name)
  end
  
end
