name: function
version: 1.11
description: Functions and combinators
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Function"

def {
  package: function-def-1.5
}

thm {
  import: def
  package: function-thm-1.9
}

main {
  import: def
  import: thm
}
