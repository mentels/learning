defmodule ModuledocFalse do
  @moduledoc false

  def f_no_doc, do: :no_doc

  @doc false
  def f_doc_false, do: :doc_false

  @doc "Ala"
  def f_normal_doc, do: :normal_doc
  
end

defmodule ModuledocTrue do
  @moduledoc "My module"

  def f_no_doc, do: :no_doc

  @doc false
  def f_doc_false, do: :doc_false

  @doc "Ala"
  def f_normal_doc, do: :normal_doc
  
end

