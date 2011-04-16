name: set
version: 1.0
description: Basic set theory
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Set"

def {
  package: set-def-1.0
}

thm {
  import: def
  package: set-thm-1.0
}

main {
  import: def
  import: thm
}
