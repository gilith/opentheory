name: parser-comb
version: 1.96
description: Stream parser combinators
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: parser-stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Parser"
show: "Parser.Stream"

def {
  package: parser-comb-def-1.89
}

thm {
  import: def
  package: parser-comb-thm-1.98
}

main {
  import: def
  import: thm
}
