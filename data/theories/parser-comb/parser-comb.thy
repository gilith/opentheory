name: parser-comb
version: 1.87
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
  package: parser-comb-def-1.82
  checksum: f2cac01e005a067185502f5cbaea69c7c62f25bf
}

thm {
  import: def
  package: parser-comb-thm-1.90
  checksum: 7e1643dcfb9933f2d59e4de6e442f8eb8126ec77
}

main {
  import: def
  import: thm
}
