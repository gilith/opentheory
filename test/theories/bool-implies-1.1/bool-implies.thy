name: bool-implies
version: 1.1
description: Boolean implication
author: Joe Programmer <joe@gilith.com>
license: MIT
requires: bool-and
requires: bool-forall
requires: bool-true
show: "Data.Bool"

def {
  package: bool-implies-def-1.0
}

thm {
  import: def
  package: bool-implies-thm-1.1
}

main {
  import: def
  import: thm
}
