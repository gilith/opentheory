name: parser
version: 1.108
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
  package: parser-stream-1.98
  checksum: b505fcdfc804f13476a100b017a3cc78fded4748
}

comb {
  import: stream
  package: parser-comb-1.89
  checksum: f65d0ecefdb39ec1924618ac88b76fa13a03595e
}

all {
  import: stream
  import: comb
  package: parser-all-1.93
  checksum: e5b13d3d5e25f8bbfb63c84dbc3437e54f8ca56f
}

main {
  import: stream
  import: comb
  import: all
}
