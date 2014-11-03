name: haskell-parser
version: 1.127
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
  package: haskell-parser-def-1.53
  checksum: cc84276a6f720708a1837d3f8a2a5cdb280bc120
}

src {
  import: def
  package: haskell-parser-src-1.95
  checksum: 8d0cf3a0faddf325122ae20a6fd6fb901c3d2613
}

test {
  import: def
  package: haskell-parser-test-1.57
  checksum: 551a1755ae31f10d06e326325144ba821662dce0
}

main {
  import: def
  import: src
  import: test
}
