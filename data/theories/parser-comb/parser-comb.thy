name: parser-comb
version: 1.28
description: Stream parser combinators
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: option
requires: parser-stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Parser"
show: "Parser.Stream"

def {
  package: parser-comb-def-1.28
}

thm {
  import: def
  package: parser-comb-thm-1.32
}

main {
  import: def
  import: thm
}
