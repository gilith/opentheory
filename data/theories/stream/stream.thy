name: stream
version: 1.35
description: Stream types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: pair
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: stream-def-1.31
  checksum: 05b4b8257a32eeabbb7ffe7287409eaf74e5ee88
}

thm {
  import: def
  package: stream-thm-1.33
  checksum: eed7d800681586b62bca601e7235682bcfbd4bff
}

main {
  import: def
  import: thm
}
