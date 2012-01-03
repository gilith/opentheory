name: char-utf8-thm
version: 1.51
description: Properties of the UTF-8 encoding of Unicode characters
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2011-12-18
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
requires: char-utf8-def
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
