name: char
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
  package: char-def-1.3
}

thm {
  import: def
  package: char-thm-1.0
}

utf8 {
  import: def
  import: thm
  package: char-utf8-1.3
}

main {
  import: def
  import: thm
  import: utf8
}
