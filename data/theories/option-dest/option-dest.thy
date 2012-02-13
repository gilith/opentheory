name: option-dest
version: 1.32
description: Option type destructors
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: option-def
requires: option-thm
show: "Data.Bool"
show: "Data.Option"

def {
  package: option-dest-def-1.35
}

thm {
  import: def
  package: option-dest-thm-1.8
}

main {
  import: def
  import: thm
}
