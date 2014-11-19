name: char
version: 1.111
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
  package: char-def-1.98
  checksum: edcc12c6c0579822d2d462c18848d542615a88a2
}

thm {
  import: def
  package: char-thm-1.17
  checksum: c42ad7b2ea4a581bd21ea5559ab740922febd8fd
}

utf8 {
  import: def
  import: thm
  package: char-utf8-1.102
  checksum: 6aee14374a31e6472b0a852d07f50689d29b6e13
}

main {
  import: def
  import: thm
  import: utf8
}
