name: parser-all
version: 1.32
description: The whole stream parser
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: natural
requires: option
requires: list
requires: parser-stream
requires: parser-comb
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"

def {
  package: parser-all-def-1.29
}

thm {
  import: def
  package: parser-all-thm-1.36
}

main {
  import: def
  import: thm
}
