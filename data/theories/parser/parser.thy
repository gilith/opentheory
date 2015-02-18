name: parser
version: 1.135
description: Stream parsers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: probability
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"
show: "Probability.Random"
show: "Relation"
haskell-int-file: haskell.int
haskell-src-file: haskell.art

stream {
  package: parser-stream-1.101
}

comb {
  import: stream
  package: parser-comb-1.94
}

all {
  import: stream
  import: comb
  package: parser-all-1.98
}

main {
  import: stream
  import: comb
  import: all
}
