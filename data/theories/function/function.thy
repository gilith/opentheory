name: function
version: 1.55
description: Function operators and combinators
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Function"

def {
  package: function-def-1.20
}

thm {
  import: def
  package: function-thm-1.49
}

main {
  import: def
  import: thm
}
