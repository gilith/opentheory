name: char-utf8-thm
version: 1.104
description: Properties of the UTF-8 encoding of Unicode characters
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2014-06-12
requires: bool
requires: byte
requires: char-def
requires: char-thm
requires: char-utf8-def
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

main {
  article: "char-utf8-thm.art"
}
