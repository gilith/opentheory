name: char
version: 1.54
description: Unicode characters
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: pair
requires: natural
requires: option
requires: list
requires: byte
requires: word16
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

def {
  package: char-def-1.45
}

thm {
  import: def
  package: char-thm-1.8
}

utf8 {
  import: def
  import: thm
  package: char-utf8-1.51
}

main {
  import: def
  import: thm
  import: utf8
}
