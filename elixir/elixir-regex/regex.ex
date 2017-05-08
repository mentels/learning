# From http://stackoverflow.com/questions/37740061/idiomatic-elixir-to-get-nested-list-item

Regex.scan(~r/^Bearer|\w+/, header_content)

Regex.run(~r/Bearer\s*(.*)/, header_content)

r = ~r/ala (?<id>\d+)/

["ala 11", "11"] = Regex.run(r, "ala 11", capture: :all)

%{"id" => "11"} = Regex.named_captures r, "ala 11", capture: :all

~r/\* \[(\w+)\]/
Regex.run(r, "* [ala] haha\n* [ola] hehe", capture: :all)
["* [ala]", "ala"]
