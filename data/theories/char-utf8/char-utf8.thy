name: char-utf8
version: 1.17
description: Theory of UTF-8 encoders and decoders
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Byte" as "Byte"
show: "Data.Char" as "Char"
show: "Data.Char.UTF8"
show: "Data.List"
show: "Data.Option" as "Option"
show: "Data.Pair"
show: "Data.Word16" as "Word16"
show: "Number.Natural" as "Natural"
show: "Number.Numeral"
show: "Parser"

def {
  package: char-utf8-def-1.17
}

thm {
  import: def
  package: char-utf8-thm-1.18
}

main {
  import: def
  import: thm
}
