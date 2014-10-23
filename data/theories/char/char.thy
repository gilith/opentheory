name: char
version: 1.108
description: Unicode characters
author: Joe Leslie-Hurd <joe@gilith.com>
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
  package: char-def-1.95
  checksum: 135bae13c6d1bb8b77a78a42667a968f830a0412
}

thm {
  import: def
  package: char-thm-1.15
  checksum: 063dbc6c539b0feacad1e307c66bc18cc00476e5
}

utf8 {
  import: def
  import: thm
  package: char-utf8-1.99
  checksum: 94163239fcc183b1a375d2340f7c164a52e0cb9b
}

main {
  import: def
  import: thm
  import: utf8
}
