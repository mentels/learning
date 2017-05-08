defmodule JsonApi.Router do
  use JsonApi.Web, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", JsonApi do
    pipe_through :api
    get "/authors/:name/books/", BookController, :index
    get "/authors/:name/", AuthorController, :show
  end
end
