name: parser-all
version: 1.86
description: The whole stream parser
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: option
requires: pair
requires: parser-comb
requires: parser-stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"

def {
  package: parser-all-def-1.84
}

thm {
  import: def
  package: parser-all-thm-1.90
}

main {
  import: def
  import: thm
}
