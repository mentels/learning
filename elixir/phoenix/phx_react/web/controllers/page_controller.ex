defmodule PhxReact.PageController do
  use PhxReact.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
