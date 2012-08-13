name: haskell-parser
version: 1.103
description: Simple stream parsers
author: Joe Hurd <joe@gilith.com>
license: MIT
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
  package: haskell-parser-def-1.44
}

src {
  import: def
  package: haskell-parser-src-1.73
}

test {
  import: def
  package: haskell-parser-test-1.34
}

main {
  import: def
  import: src
  import: test
}
