name: bool-and
version: 1.0
description: Boolean conjunction
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool-forall
requires: bool-true
show: "Data.Bool"

def {
  package: bool-and-def-1.0
}

thm {
  import: def
  package: bool-and-thm-1.0
}

main {
  import: def
  import: thm
}
