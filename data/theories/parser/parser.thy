name: parser
version: 1.18
description: Basic theory of parsers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Parser"
show: "Relation"

stream {
  package: parser-stream-1.17
}

comb {
  import: stream
  package: parser-comb-1.11
}

all {
  import: stream
  import: comb
  package: parser-all-1.14
}

main {
  import: stream
  import: comb
  import: all
}
