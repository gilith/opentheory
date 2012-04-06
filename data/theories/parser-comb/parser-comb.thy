name: parser-comb
version: 1.53
description: Stream parser combinators
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: option
requires: pair
requires: parser-stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Parser"
show: "Parser.Stream"

def {
  package: parser-comb-def-1.49
}

thm {
  import: def
  package: parser-comb-thm-1.57
}

main {
  import: def
  import: thm
}
