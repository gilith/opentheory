name: char
version: 1.139
description: Unicode characters
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: byte
requires: natural-bits
requires: parser
requires: probability
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Char"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Sum"
show: "Function"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"
show: "Probability.Random"
show: "Set"
haskell-name: opentheory-unicode
haskell-category: Text
haskell-int-file: haskell.int
haskell-src-file: haskell.art
haskell-test-file: haskell-test.art
haskell-equality-type: "Data.Char.char"
haskell-arbitrary-type: "Data.Char.char"

def {
  package: char-def-1.106
}

thm {
  import: def
  package: char-thm-1.23
}

utf8 {
  import: def
  import: thm
  package: char-utf8-1.115
}

main {
  import: def
  import: thm
  import: utf8
}
