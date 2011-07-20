name: bool-true-axiom
version: 1.0
description: Definition and assertion about the boolean true constant
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

def {
  package: bool-true-def-1.0
}

assert {
  import: def
  package: bool-true-assert-1.0
}

main {
  import: def
  import: assert
}
