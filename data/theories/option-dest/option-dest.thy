name: option-dest
version: 1.24
description: Option type destructors
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: option-def
requires: option-thm
show: "Data.Bool"
show: "Data.Option"

def {
  package: option-dest-def-1.25
}

thm {
  import: def
  package: option-dest-thm-1.7
}

main {
  import: def
  import: thm
}
