name: real
version: 1.56
description: The real numbers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
requires: pair
requires: set
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Number.Real"
show: "Set"

def {
  package: real-def-1.68
}

thm {
  import: def
  package: real-thm-1.44
}

main {
  import: def
  import: thm
}
