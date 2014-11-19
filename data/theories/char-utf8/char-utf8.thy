name: char-utf8
version: 1.102
description: The UTF-8 encoding of Unicode characters
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: byte
requires: char-def
requires: char-thm
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
  package: char-utf8-def-1.89
  checksum: b302193541b4a0e41c039f8d2e20fac75fd9d2ca
}

thm {
  import: def
  package: char-utf8-thm-1.108
  checksum: 2f138180e8d82c4d54f32128158b0c7cee753547
}

main {
  import: def
  import: thm
}
