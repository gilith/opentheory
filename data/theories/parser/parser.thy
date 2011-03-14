name: parser
version: 1.1
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
  package: parser-stream-1.1
}

basic {
  import: stream
  package: parser-basic-1.1
}

rec {
  import: stream
  import: basic
  package: parser-rec-1.1
}

main {
  import: stream
  import: basic
  import: rec
}
