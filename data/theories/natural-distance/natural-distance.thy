name: natural-distance
version: 1.7
description: Natural number distance function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-distance-def-1.10
}

thm {
  import: def
  package: natural-distance-thm-1.11
}

main {
  import: def
  import: thm
}
