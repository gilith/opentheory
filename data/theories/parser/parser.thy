name: parser
version: 1.162
description: Stream parsers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
homepage: https://opentheory.gilith.com/?pkg=parser
requires: base
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Sum"
show: "Function"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"
show: "Relation"
hol-light-int-file: hol-light.int
hol-light-thm-file: hol-light.art
haskell-category: Parsing
haskell-int-file: haskell.int
haskell-src-file: haskell.art
haskell-test-file: haskell-test.art
haskell-equality-type: "Parser.Stream.stream"
haskell-arbitrary-type: "Parser.Stream.stream"

stream {
  package: parser-stream-1.111
}

comb {
  import: stream
  package: parser-comb-1.99
}

fold {
  import: stream
  import: comb
  package: parser-fold-1.2
}

all {
  import: stream
  import: comb
  package: parser-all-1.103
}

main {
  import: stream
  import: comb
  import: fold
  import: all
}
