name: parser-stream
version: 1.97
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
  package: parser-stream-def-1.95
  checksum: 60c72d68bc25cf3a3a9eb8516d6bf56a6dc892cf
}

thm {
  import: def
  package: parser-stream-thm-1.95
  checksum: ac50cde509f3f4d1cc3b4dc6d22657d8e20b450a
}

main {
  import: def
  import: thm
}
