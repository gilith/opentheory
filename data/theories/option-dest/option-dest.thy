name: option-dest
version: 1.0
description: Theory of the option destructors
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Option"

def {
  package: option-dest-def-1.0
}

thm {
  import: def
  package: option-dest-thm-1.0
}

main {
  import: def
  import: thm
}
