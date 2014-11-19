name: parser-comb
version: 1.89
description: Stream parser combinators
author: Joe Leslie-Hurd <joe@gilith.com>
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
  package: parser-comb-def-1.83
  checksum: fcd38ea1bffc7be4eb681d09fa66b22ef4a97266
}

thm {
  import: def
  package: parser-comb-thm-1.91
  checksum: 6a85ae033ae9e7572facc4b6ef63ab11585d0cfc
}

main {
  import: def
  import: thm
}
