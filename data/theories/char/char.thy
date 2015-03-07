name: char
version: 1.117
description: Unicode characters
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: byte
requires: parser
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.Char"
show: "Data.Char.UTF8"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Word16"
show: "Data.Word16.Bits"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"
haskell-int-file: haskell.int
haskell-src-file: haskell.art

def {
  package: char-def-1.100
}

thm {
  import: def
  package: char-thm-1.19
}

utf8 {
  import: thm
  package: char-utf8-1.104
}

main {
  import: def
  import: thm
  import: utf8
}
