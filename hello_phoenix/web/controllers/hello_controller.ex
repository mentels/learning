# The first part of the view's name associated with this controller
# must match HelloPhoenix
defmodule HelloPhoenix.HelloController do
  use HelloPhoenix.Web, :controller

  # This is an action
  def index(conn, _params) do
    # Expects to find the template in web/templates/hello/index.html.eex
    render conn, "index.html"
  end

  def show(conn, %{"messenger" => messenger}) do
    render conn, "show.html", messenger: messenger
  end
end
