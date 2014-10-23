name: parser
version: 1.103
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
  package: parser-stream-1.94
  checksum: 20ef6509e8d3100f612dbfbff895bdb594741391
}

comb {
  import: stream
  package: parser-comb-1.85
  checksum: 6dab991fa91c16425d4d420d6b412c4a8295d384
}

all {
  import: stream
  import: comb
  package: parser-all-1.89
  checksum: abd677d78bfb4ded7be169e2f0bec6ad427f872c
}

main {
  import: stream
  import: comb
  import: all
}
