name: option-dest
version: 1.14
description: Theory of the option destructors
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Option"

def {
  package: option-dest-def-1.16
}

thm {
  import: def
  package: option-dest-thm-1.2
}

main {
  import: def
  import: thm
}
