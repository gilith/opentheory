name: function
version: 1.50
description: Function operators and combinators
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Function"

def {
  package: function-def-1.17
}

thm {
  import: def
  package: function-thm-1.45
}

main {
  import: def
  import: thm
}
