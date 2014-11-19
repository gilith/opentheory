name: parser-stream
version: 1.98
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
  package: parser-stream-thm-1.96
  checksum: 437d6bbb749b753568a4be5ce7558d071d360cc0
}

main {
  import: def
  import: thm
}
