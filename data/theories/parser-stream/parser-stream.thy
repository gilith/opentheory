name: parser-stream
version: 1.74
description: Parse streams
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: option
requires: pair
requires: relation
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Parser.Stream"
show: "Relation"

def {
  package: parser-stream-def-1.76
}

thm {
  import: def
  package: parser-stream-thm-1.76
}

main {
  import: def
  import: thm
}
