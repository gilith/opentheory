name: option-dest
version: 1.46
description: Option type destructors
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: option-def
requires: option-thm
show: "Data.Bool"
show: "Data.Option"

def {
  package: option-dest-def-1.51
}

thm {
  import: def
  package: option-dest-thm-1.10
}

main {
  import: def
  import: thm
}
