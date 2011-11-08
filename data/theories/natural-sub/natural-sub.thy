name: natural-sub
version: 1.7
description: Definitions and theorems about natural number subtraction
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-sub-def-1.6
}

thm {
  import: def
  package: natural-sub-thm-1.6
}

main {
  import: def
  import: thm
}
