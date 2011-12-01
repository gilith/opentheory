name: parser
version: 1.40
description: Stream parsers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: pair
requires: natural
requires: relation
requires: option
requires: list
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"
show: "Relation"

stream {
  package: parser-stream-1.36
}

comb {
  import: stream
  package: parser-comb-1.28
}

all {
  import: stream
  import: comb
  package: parser-all-1.32
}

main {
  import: stream
  import: comb
  import: all
}
