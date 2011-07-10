name: real
version: 1.0
description: The real numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Numeral"
show: "Number.Real"
show: "Set"

def {
  package: real-def-1.4
}

thm {
  import: def
  package: real-thm-1.0
}

main {
  import: def
  import: thm
}
