name: natural-factorial
version: 1.5
description: Definitions and theorems about natural number factorial
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-factorial-def-1.5
}

thm {
  import: def
  package: natural-factorial-thm-1.4
}

main {
  import: def
  import: thm
}
