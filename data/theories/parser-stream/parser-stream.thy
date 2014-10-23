name: parser-stream
version: 1.94
description: Parse streams
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: option
requires: pair
requires: probability
requires: relation
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Parser.Stream"
show: "Probability.Random"
show: "Relation"

def {
  package: parser-stream-def-1.92
  checksum: 014a3d178251eea372058b9f686b4baa892ac518
}

thm {
  import: def
  package: parser-stream-thm-1.93
  checksum: 7f23c6b77857fad33a8c51b49dbe2eee3ea1ea80
}

main {
  import: def
  import: thm
}
