name: parser-all
version: 1.71
description: The whole stream parser
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
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
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"

def {
  package: parser-all-def-1.69
}

thm {
  import: def
  package: parser-all-thm-1.75
}

main {
  import: def
  import: thm
}
