name: set-finite
version: 1.0
description: Finite set theory
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Set"

def {
  package: set-finite-def-1.0
}

thm {
  import: def
  package: set-finite-thm-1.0
}

main {
  import: def
  import: thm
}
