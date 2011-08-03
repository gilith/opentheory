name: natural-sub
version: 1.5
description: Definitions and theorems about natural number subtraction
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-sub-def-1.4
}

thm {
  import: def
  package: natural-sub-thm-1.4
}

main {
  import: def
  import: thm
}
