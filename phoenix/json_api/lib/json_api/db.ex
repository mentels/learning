defmodule JsonApi.Db do

  def books(_) do
    titles = ["7 languages in 7 weeks",
              "7 Habits of Highly Effective People",
              "Heroic Leadership"]
    for t <- titles, do: %{title: t, isbn: Enum.sum to_charlist(t)}
  end
  
end
