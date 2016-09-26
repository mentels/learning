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
      books: render_many(books(), JsonApi.BookView, "book.json")}
  end

  defp books() do
    titles = ["7 languages in 7 weeks",
              "7 Habits of Highly Effective People",
              "Heroic Leadership"]
    for t <- titles, do: %{title: t, isbn: Enum.sum to_charlist(t)}
  end
      
end
