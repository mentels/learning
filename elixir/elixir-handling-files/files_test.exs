ExUnit.start

defmodule HandlingFilesTests do
  use ExUnit.Case

  @in_file "in.txt"
  @contents """
  Lofoten Islands is an amazing island group in Norway.
  They are situated within the Arctic Circle.
  I love them!
  """

  setup do
    on_exit fn -> File.rm(@in_file) end
  end

  setup context do
    {:ok, file} = File.open @in_file, [:write]
    IO.binwrite file, @contents
    {:ok, in_file: @in_file, contents: @contents}
  end

  test "File.stream!/1 allows to stream from a file
  (as default by line)", %{in_file: f} do
    stream = File.stream!(f) |> Enum.map(&String.strip(&1))
    assert Enum.take(stream, 2) == (File.read!(f)
                                    |> String.split("\n")
                                    |> Enum.slice(0..1))
  end

  test "File.stream!/1 allows to stream a file line by line",
  %{in_file: f} do
    contents = File.read!(f) |> String.split("\n")
    stream = File.stream!(f, [:utf8], :line)
    Enum.each stream, fn line ->
      assert String.strip(line) in contents
    end
  end

  test "IO.stream/2 allows to stream a file line by line",
  %{in_file: f, contents: c} do
    stream = File.open!(f) |> IO.stream(:line)
    Enum.each stream, &(assert String.strip(&1) in String.split(c,
                                                                "\n"))
  end

  test "IO.stream/2 implements higher abstraction than Stream.resource/3",
    %{in_file: f} do
    io_stream = File.open!(f) |> IO.stream(:line)
    raw_stream = Stream.resource(
      fn -> File.open!(f) end,
      fn file ->
        case IO.read(file, :line) do
          data when is_binary(data) -> {[data], file}
          _ -> {:halt, file}
        end
      end,
      fn file -> File.close(file) end)
    assert Enum.into(io_stream, []) == Enum.into(raw_stream, [])
  end

  test "IO.binstream/2 allows to stream a file line by line",
  %{in_file: f, contents: c} do
    stream = File.open(f) |> elem(1) |> IO.binstream(:line)
    Enum.each stream, &(assert String.strip(&1) in String.split(c,
                                                                "\n"))
  end

  test "Using File.{read, write} doesn't require opening a file",
  %{in_file: f, contents: c} do
    assert {:ok, c} == File.read(f)
    File.write(f, c, [:write])
    assert {:ok, c} == File.read(f)
  end
  
end

defmodule FilesEncodingTests do
  use ExUnit.Case

  @in_file "encoding.txt"
  @contents """
  Chrząszcz rżnie źdźbło a tam łąbędź patrzy i myśli: 'Nie wierzę!'"
  """

  setup do
    on_exit fn -> File.rm(@in_file) end
  end

  test "IO.binwrite/3 writes the data as binary and IO.binread/2 reads
  binary; no unicode conversion involved" do
    {:ok, file} = File.open @in_file, [:write]
    IO.binwrite file, @contents
    :ok = File.close file
    {:ok, file} = File.open @in_file, [:read]
    assert IO.binread(file, :all) == @contents
  end

  test "IO.{write ,read}/2 don't work with files in binary mode" do
    {:ok, file} = File.open @in_file, [:write]
    assert_raise ErlangError, fn -> IO.write file, @contents end
  end

  test "IO.{write ,read}/2 work with files in utf8 mode" do
    {:ok, file} = File.open @in_file, [:write, :utf8]
    IO.write file, @contents
    File.close file
    {:ok, file} = File.open @in_file, [:read, :utf8]
    assert IO.read(file, :all) == @contents
  end

  test "File.write/1 doesn't work with :utf8" do
    assert {:error, :badarg} == File.write(@in_file, @contents, [:utf8])
  end

  test "File.read/1 does work with :utf8 files" do
    {:ok, file} = File.open @in_file, [:write, :utf8]
    IO.write file, @contents
    File.close file
    assert {:ok, @contents} == File.read(@in_file)
  end

  test "File.read/1 does work with binary files" do
    {:ok, file} = File.open @in_file, [:write]
    IO.binwrite file, @contents
    File.close file
    assert {:ok, @contents} == File.read(@in_file)
  end
  
end
