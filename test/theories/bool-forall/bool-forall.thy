name: bool-forall
version: 1.0
description: The universal quantifier
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool-true
show: "Data.Bool"

def {
  package: bool-forall-def-1.0
}

thm {
  import: def
  package: bool-forall-thm-1.0
}

main {
  import: def
  import: thm
}
