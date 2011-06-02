name: set-size
version: 1.0
description: Sizes of finite sets
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Set"

def {
  package: set-size-def-1.0
}

thm {
  import: def
  package: set-size-thm-1.0
}

main {
  import: def
  import: thm
}
