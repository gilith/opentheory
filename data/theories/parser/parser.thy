name: parser
version: 1.22
description: Theory of simple parsers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Number.Natural"
show: "Parser"
show: "Relation"

stream {
  package: parser-stream-1.20
}

comb {
  import: stream
  package: parser-comb-1.13
}

all {
  import: stream
  import: comb
  package: parser-all-1.17
}

main {
  import: stream
  import: comb
  import: all
}
