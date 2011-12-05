name: parser-stream
version: 1.39
description: Parse streams
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
show: "Parser.Stream"
show: "Relation"

def {
  package: parser-stream-def-1.40
}

thm {
  import: def
  package: parser-stream-thm-1.41
}

main {
  import: def
  import: thm
}
