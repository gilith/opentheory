name: parser-stream
version: 1.20
description: Basic theory of parse streams
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Parser.Stream"
show: "Relation"

def {
  package: parser-stream-def-1.20
}

thm {
  import: def
  package: parser-stream-thm-1.21
}

main {
  import: def
  import: thm
}
