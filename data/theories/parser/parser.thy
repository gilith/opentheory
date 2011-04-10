name: parser
version: 1.7
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
  package: parser-stream-1.7
}

comb {
  import: stream
  package: parser-comb-1.0
}

all {
  import: stream
  import: comb
  package: parser-all-1.4
}

main {
  import: stream
  import: comb
  import: all
}
