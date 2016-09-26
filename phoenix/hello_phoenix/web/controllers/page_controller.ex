defmodule HelloPhoenix.PageController do
  use HelloPhoenix.Web, :controller

  def index(conn, _params) do
    # render conn, "index.html"
    pages = for t <- ["foo", "bar"], do: %{title: t}
    render conn, "index.json", pages: pages
  end

  def show(conn, _params) do
    page = %{title: "foo"}
    render conn, "show.json", page: page
  end
end
