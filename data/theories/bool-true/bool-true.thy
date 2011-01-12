name: bool-true
version: 1.0
description: Definition and theorem about the boolean true constant
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

def {
  package: bool-true-def-1.0
}

thm {
  import: def
  package: bool-true-thm-1.0
}

main {
  import: def
  import: thm
}
