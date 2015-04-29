name: parser-stream
version: 1.110
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
  package: parser-stream-def-1.100
}

thm {
  import: def
  package: parser-stream-thm-1.106
}

main {
  import: def
  import: thm
}
