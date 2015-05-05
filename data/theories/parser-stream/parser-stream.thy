name: parser-stream
version: 1.111
description: Parse streams
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Parser.Stream"
show: "Relation"

def {
  package: parser-stream-def-1.101
}

thm {
  import: def
  package: parser-stream-thm-1.107
}

main {
  import: def
  import: thm
}
