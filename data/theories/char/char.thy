name: char
version: 1.0
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

def {
  package: char-def-1.0
}

utf8 {
  import: def
  package: char-utf8-1.0
}

main {
  import: def
  import: utf8
}
