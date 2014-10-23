name: haskell-parser
version: 1.125
description: Stream parsers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
haskell-category: Parsing
requires: base
requires: haskell
requires: parser
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Function"
show: "Haskell.Data.List" as "H"
show: "Haskell.Data.Option" as "H"
show: "Haskell.Number.Natural" as "H"
show: "Haskell.Parser" as "H"
show: "Number.Natural"
show: "Parser"
show: "Probability.Random"

def {
  package: haskell-parser-def-1.51
  checksum: 9aa2b6fb6f2fb5f6cae19dd9631090f738125922
}

src {
  import: def
  package: haskell-parser-src-1.93
  checksum: e2f6b5fff5d8f7291473ae9bfc6ce2f35651d26b
}

test {
  import: def
  package: haskell-parser-test-1.55
  checksum: acdeef97454ae99a288e02f9800b84522239cd34
}

main {
  import: def
  import: src
  import: test
}
