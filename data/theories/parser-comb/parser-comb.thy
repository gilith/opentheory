name: parser-comb
version: 1.85
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
  package: parser-comb-def-1.80
  checksum: 7fea0ae4d2415c1fe604048626effd06c11d3ff1
}

thm {
  import: def
  package: parser-comb-thm-1.88
  checksum: 3c7032a7e6bb217b1eccde3c0909e8cf9d882ff2
}

main {
  import: def
  import: thm
}
