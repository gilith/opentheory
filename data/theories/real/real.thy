name: real
version: 1.27
description: The real numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: pair
requires: natural
requires: set
show: "Data.Bool"
show: "Data.Pair"
show: "Function"
show: "Number.Natural" as "Natural"
show: "Number.Real"
show: "Set"

def {
  package: real-def-1.35
}

thm {
  import: def
  package: real-thm-1.22
}

main {
  import: def
  import: thm
}
