name: parser
version: 1.144
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
haskell-int-file: haskell.int
haskell-src-file: haskell.art

stream {
  package: parser-stream-1.105
}

comb {
  import: stream
  package: parser-comb-1.97
}

fold {
  import: stream
  import: comb
  package: parser-fold-1.1
}

all {
  import: stream
  import: comb
  package: parser-all-1.101
}

main {
  import: stream
  import: comb
  import: fold
  import: all
}
