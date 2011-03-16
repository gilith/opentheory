name: char-utf8
version: 1.3
description: Theory of Unicode characters
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Byte" as "Byte"
show: "Data.Char"
show: "Data.List"
show: "Data.Pair"
show: "Data.Word16" as "Word16"
show: "Number.Numeral"
show: "Parser"

def {
  package: char-utf8-def-1.3
}

thm {
  import: def
  package: char-utf8-thm-1.3
}

main {
  import: def
  import: thm
}
