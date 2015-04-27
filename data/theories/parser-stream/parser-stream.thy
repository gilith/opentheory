name: parser-stream
version: 1.108
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
  package: parser-stream-def-1.100
}

thm {
  import: def
  package: parser-stream-thm-1.105
}

main {
  import: def
  import: thm
}
