defmodule Templates.PageController do
  use Templates.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end

  def test(conn, _params) do
    raise "My error!"
    render conn, "test.html"
  end
end
