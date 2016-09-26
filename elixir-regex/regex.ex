# From http://stackoverflow.com/questions/37740061/idiomatic-elixir-to-get-nested-list-item

Regex.scan(~r/^Bearer|\w+/, header_content)

Regex.run(~r/Bearer\s*(.*)/, header_content)
