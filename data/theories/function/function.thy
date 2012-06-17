name: function
version: 1.37
description: Function operators and combinators
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Function"

def {
  package: function-def-1.14
}

thm {
  import: def
  package: function-thm-1.33
}

main {
  import: def
  import: thm
}
