name: haskell-char
version: 1.49
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
  package: haskell-char-def-1.20
  checksum: 2177a53d7e375e5a062b3af91f093155bb05cd79
}

src {
  import: def
  package: haskell-char-src-1.47
  checksum: a58e4640a7aac50e1fa184969ae4c2e44eb28150
}

test {
  import: def
  package: haskell-char-test-1.35
  checksum: af0c6e862768fe24193d8b7ed7ec2530eeebf17b
}

main {
  import: def
  import: src
  import: test
}
