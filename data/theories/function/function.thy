name: function
version: 1.4
description: Basic theory of functions
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Function"

def {
  package: function-def-1.1
}

thm {
  import: def
  package: function-thm-1.4
}

main {
  import: def
  import: thm
}
