name: function
version: 1.8
description: Basic theory of functions
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Function"

def {
  package: function-def-1.2
}

thm {
  import: def
  package: function-thm-1.8
}

main {
  import: def
  import: thm
}
