name: char-utf8
version: 1.42
description: The UTF-8 encoding of Unicode characters
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
requires: char-def
requires: char-thm
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
  package: char-utf8-def-1.33
}

thm {
  import: def
  package: char-utf8-thm-1.45
}

main {
  import: def
  import: thm
}
