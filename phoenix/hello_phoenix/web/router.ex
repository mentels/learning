defmodule HelloPhoenix.Router do
  use HelloPhoenix.Web, :router

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

  scope "/", HelloPhoenix do
    pipe_through :browser # Use the default browser stack

    get "/", PageController, :index
    get "/:page", PageController, :show
    # It will look for HelloPhoenix.HelloController module and invoke
    # the index action which is a functin index/2
    get "/hello", HelloController, :index
    get "/hello/:messenger", HelloController, :show

    # the resources macro will expand to 8 match/3 functions to support
    # 8 actions: index, edit, new, show, create, update x2 (PATCH,PUT)
    # and delete
    resources "/users", UserController
    resources "/videos", VideoController
    # resources "/posts", PostController, only: [:index, :show]
    # resources "/comments", CommentController, except: [:delete]
  end

  # Other scopes may use custom stacks.
  # scope "/api", HelloPhoenix do
  #   pipe_through :api
  # end
end
