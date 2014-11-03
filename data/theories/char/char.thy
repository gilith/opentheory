name: char
version: 1.110
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
  package: char-def-1.97
  checksum: 0acd7e7943b55293a906158425430f9d5d8bbf82
}

thm {
  import: def
  package: char-thm-1.17
  checksum: c42ad7b2ea4a581bd21ea5559ab740922febd8fd
}

utf8 {
  import: def
  import: thm
  package: char-utf8-1.101
  checksum: 55a0a2bdde6a28d62a526316149ec204df28c59f
}

main {
  import: def
  import: thm
  import: utf8
}
