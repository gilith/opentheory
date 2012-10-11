name: parser-stream
version: 1.85
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
  package: parser-stream-def-1.85
}

thm {
  import: def
  package: parser-stream-thm-1.85
}

main {
  import: def
  import: thm
}
