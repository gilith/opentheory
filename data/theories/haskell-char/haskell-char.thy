name: haskell-char
version: 1.51
description: Unicode characters
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
haskell-category: Text
requires: base
requires: char
requires: haskell
requires: haskell-parser
requires: parser
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Char"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Word16"
show: "Function"
show: "Haskell.Data.Byte" as "H"
show: "Haskell.Data.List" as "H"
show: "Haskell.Data.Option" as "H"
show: "Haskell.Data.Unicode" as "H"
show: "Haskell.Data.Word16" as "H"
show: "Haskell.Number.Natural" as "H"
show: "Haskell.Parser" as "H"
show: "Number.Natural"
show: "Parser"
show: "Probability.Random"

def {
  package: haskell-char-def-1.22
  checksum: cddc8ecae3251d732ffb1ddc320e57dbfa687dc3
}

src {
  import: def
  package: haskell-char-src-1.49
  checksum: cea106da4c0fd867c5d8963780370257c1b9adbb
}

test {
  import: def
  package: haskell-char-test-1.37
  checksum: 528243e529957aad50f20217478f944ea9844507
}

main {
  import: def
  import: src
  import: test
}
