name: real
version: 1.40
description: The real numbers
author: Joe Hurd <joe@gilith.com>
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
  package: real-def-1.49
}

thm {
  import: def
  package: real-thm-1.31
}

main {
  import: def
  import: thm
}
