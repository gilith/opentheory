name: char
version: 1.123
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
show: "Data.Char.UTF8"
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
haskell-int-file: haskell.int
haskell-src-file: haskell.art

def {
  package: char-def-1.104
}

thm {
  import: def
  package: char-thm-1.21
}

utf8 {
  import: def
  package: char-utf8-1.105
}

main {
  import: def
  import: thm
  import: utf8
}
