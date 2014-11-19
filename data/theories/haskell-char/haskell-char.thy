name: haskell-char
version: 1.52
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
  package: haskell-char-def-1.23
  checksum: 7722f496f504625319b74ecb1e94003941443cb6
}

src {
  import: def
  package: haskell-char-src-1.50
  checksum: 5b7b3609194fc0a1921c46fd9a8c82ec547fe6a5
}

test {
  import: def
  package: haskell-char-test-1.38
  checksum: ef63c200dcbe54992165deb7c8a13ecc32c59798
}

main {
  import: def
  import: src
  import: test
}
