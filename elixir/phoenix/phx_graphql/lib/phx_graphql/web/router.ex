defmodule PhxGraphql.Web.Router do
  use PhxGraphql.Web, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", PhxGraphql.Web do
    pipe_through :api
    resources "/users", UserController, except: [:new, :edit]
    resources "/posts", PostController, except: [:new, :edit]
  end

  forward "/graph", Absinthe.Plug, schema: PhxGraphql.Schema
  forward "/graphiql", Absinthe.Plug.GraphiQL, schema: PhxGraphql.Schema
end
