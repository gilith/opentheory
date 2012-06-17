name: parser-all
version: 1.67
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
  package: parser-all-def-1.64
}

thm {
  import: def
  package: parser-all-thm-1.70
}

main {
  import: def
  import: thm
}
