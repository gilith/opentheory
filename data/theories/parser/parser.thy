name: parser
version: 1.106
description: Stream parsers
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
show: "Parser"
show: "Parser.Stream"
show: "Probability.Random"
show: "Relation"

stream {
  package: parser-stream-1.97
  checksum: c5e9fe1ec838e1b4341e0d2339f3a6e3f1d2c073
}

comb {
  import: stream
  package: parser-comb-1.88
  checksum: cbcda9881a0dcff4db43e0e3afead65be75db1cd
}

all {
  import: stream
  import: comb
  package: parser-all-1.92
  checksum: 3f6841a20002f0c21b47856e7a9f8832b7073727
}

main {
  import: stream
  import: comb
  import: all
}
