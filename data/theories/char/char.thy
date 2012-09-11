name: char
version: 1.95
description: Unicode characters
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: byte
requires: list
requires: natural
requires: option
requires: pair
requires: parser
requires: word16
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

def {
  package: char-def-1.83
}

thm {
  import: def
  package: char-thm-1.11
}

utf8 {
  import: def
  import: thm
  package: char-utf8-1.86
}

main {
  import: def
  import: thm
  import: utf8
}
