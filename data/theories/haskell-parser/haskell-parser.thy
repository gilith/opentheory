name: haskell-parser
version: 1.77
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
show: "Haskell.Parser"
show: "Number.Natural"

def {
  package: haskell-parser-def-1.30
}

thm {
  import: def
  package: haskell-parser-thm-1.26
}

src {
  import: def
  import: thm
  package: haskell-parser-src-1.50
}

test {
  import: def
  import: thm
  package: haskell-parser-test-1.10
}

main {
  import: def
  import: thm
  import: src
  import: test
}
