name: word-bits
version: 1.1
description: Parametric theory of word bit lists
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Word"
show: "Number.Numeral"

def {
  package: word-bits-def-1.0
}

thm {
  import: def
  package: word-bits-thm-1.1
}

main {
  import: def
  import: thm
}
