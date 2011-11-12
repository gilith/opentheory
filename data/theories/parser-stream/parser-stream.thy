name: parser-stream
version: 1.22
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
  package: parser-stream-def-1.22
}

thm {
  import: def
  package: parser-stream-thm-1.23
}

main {
  import: def
  import: thm
}
