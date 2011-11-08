name: option
version: 1.19
description: Basic theory of option types
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Option"
show: "Number.Natural"

def {
  package: option-def-1.12
}

thm {
  import: def
  package: option-thm-1.11
}

dest {
  import: def
  import: thm
  package: option-dest-1.16
}

main {
  import: def
  import: thm
  import: dest
}
