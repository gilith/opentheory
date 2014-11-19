name: parser-all
version: 1.93
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
  package: parser-all-thm-1.96
  checksum: 533a1ec4ca9eb0d7870c2d38136abfbb96f115d9
}

main {
  import: def
  import: thm
}
