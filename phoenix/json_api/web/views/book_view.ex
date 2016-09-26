defmodule JsonApi.BookView do
  use JsonApi.Web, :view

  def render("show.json", %{data: books}) do
    %{data: render_many(books, JsonApi.BookView, "book.json")}
  end

  def render("book.json", %{book: book}) do
    %{title: book.title, isbn: book.isbn}
  end
                                          
end
