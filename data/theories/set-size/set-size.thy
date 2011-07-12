name: set-size
version: 1.3
description: Sizes of finite sets
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Set"

def {
  package: set-size-def-1.6
}

thm {
  import: def
  package: set-size-thm-1.6
}

main {
  import: def
  import: thm
}
