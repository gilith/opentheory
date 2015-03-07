name: char-utf8
version: 1.105
description: The UTF-8 encoding of Unicode characters
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: byte
requires: char-def
requires: parser
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Char"
show: "Data.Char.UTF8"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Sum"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"

def {
  package: char-utf8-def-1.94
}

thm {
  import: def
  package: char-utf8-thm-1.112
}

main {
  import: def
  import: thm
}
