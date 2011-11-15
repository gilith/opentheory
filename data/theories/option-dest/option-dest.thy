name: option-dest
version: 1.20
description: Theory of the option destructors
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Option"

def {
  package: option-dest-def-1.22
}

thm {
  import: def
  package: option-dest-thm-1.4
}

main {
  import: def
  import: thm
}
