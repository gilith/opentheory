name: parser-all
version: 1.46
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
  package: parser-all-def-1.43
}

thm {
  import: def
  package: parser-all-thm-1.49
}

main {
  import: def
  import: thm
}
