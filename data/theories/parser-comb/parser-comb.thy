name: parser-comb
version: 1.58
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
  package: parser-comb-def-1.54
}

thm {
  import: def
  package: parser-comb-thm-1.62
}

main {
  import: def
  import: thm
}
