name: option-dest
version: 1.50
description: Option type destructors
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: option-def
requires: option-thm
show: "Data.Bool"
show: "Data.Option"

def {
  package: option-dest-def-1.55
}

thm {
  import: def
  package: option-dest-thm-1.11
}

main {
  import: def
  import: thm
}
