name: haskell-parser
version: 1.47
description: Simple stream parsers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: base
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Haskell.Parser"
show: "Number.Natural"

def {
  package: haskell-parser-def-1.2
}

src {
  import: def
  package: haskell-parser-src-1.22
}

test {
  import: def
  package: haskell-parser-test-1.4
}

main {
  import: def
  import: src
  import: test
}
