name: set-size
version: 1.12
description: Sizes of finite sets
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Set"

def {
  package: set-size-def-1.12
}

thm {
  import: def
  package: set-size-thm-1.15
}

main {
  import: def
  import: thm
}
