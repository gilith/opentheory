name: parser-all
version: 1.92
description: The whole stream parser
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: option
requires: pair
requires: parser-comb
requires: parser-stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"

def {
  package: parser-all-def-1.90
  checksum: 3658f9e60de4a861dbcba7a2f59bee1960633787
}

thm {
  import: def
  package: parser-all-thm-1.95
  checksum: 720b3e9ab15ffc418ae8d03ac949ff1b4ec8780c
}

main {
  import: def
  import: thm
}
