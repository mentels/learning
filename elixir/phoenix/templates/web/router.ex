defmodule Templates.Router do
  use Templates.Web, :router
  use FmpPlugErrorHandler, app: "templates"
  # use Plug.ErrorHandler

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/", Templates do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    get "/test", PageController, :test
  end

  scope "/" do
    forward "/health", FmpPlugServiceHealth, app: "as"
  end

  # This is not working due to the fact we cannot use get in this context
  # get "/health" do
  #   FmpPlugServiceHealth.send_health_status(conn, app: "analytics_sevice")
  # end

  # Other scopes may use custom stacks.
  # scope "/api", Templates do
  #   pipe_through :api
  # end

  # defp handle_errors(conn, %{kind: kind, reason: reason, stack: _}) do
  #   IO.puts """
  #   handle_errors: 
  #   kind ~> #{inspect kind} 
  #   reason ~> #{inspect reason}
  #   status ~> #{inspect conn.status}
  #   """
  # end
end
