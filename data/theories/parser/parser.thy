name: parser
version: 1.149
description: Stream parsers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: natural-fibonacci
requires: probability
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Sum"
show: "Function"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"
show: "Probability.Random"
show: "Relation"
haskell-category: Parsing
haskell-int-file: haskell.int
haskell-src-file: haskell.art
haskell-equality-type: "Parser.Stream.stream"

stream {
  package: parser-stream-1.107
}

comb {
  import: stream
  package: parser-comb-1.98
}

fold {
  import: stream
  import: comb
  package: parser-fold-1.2
}

all {
  import: stream
  import: comb
  package: parser-all-1.102
}

main {
  import: stream
  import: comb
  import: fold
  import: all
}
