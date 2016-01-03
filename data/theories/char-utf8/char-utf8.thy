name: char-utf8
version: 1.116
description: The UTF-8 encoding of Unicode characters
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: byte
requires: char-def
requires: char-thm
requires: natural-bits
requires: parser
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Char"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Sum"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"

def {
  package: char-utf8-def-1.96
}

thm {
  import: def
  package: char-utf8-thm-1.122
}

main {
  import: def
  import: thm
}
