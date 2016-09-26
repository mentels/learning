defmodule JsonApi.AuthorView do
  use JsonApi.Web, :view

  def render("show.json", %{data: author}) do
    # The `author` map is put into the %{author: author} when passing
    # to another call to render/2 for "author.json". The `author` name
    # comes from the AuthorView/authors router path/?
    %{data: render_one(author, JsonApi.AuthorView, "author.json")}
  end

  def render("author.json", %{author: author} = _params) do
    # IO.puts inspect(params)
    %{name: author[:name],
      surname: author.surname,
      books: render_many(author.books, JsonApi.BookView, "book.json")}
  end

end
