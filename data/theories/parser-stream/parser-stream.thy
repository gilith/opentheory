name: parser-stream
version: 1.106
description: Parse streams
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: natural-fibonacci
requires: probability
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Parser.Stream"
show: "Probability.Random"
show: "Relation"

def {
  package: parser-stream-def-1.98
}

thm {
  import: def
  package: parser-stream-thm-1.104
}

main {
  import: def
  import: thm
}
