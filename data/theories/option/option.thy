name: option
version: 1.23
description: Basic theory of option types
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Option"
show: "Number.Natural"

def {
  package: option-def-1.20
}

thm {
  import: def
  package: option-thm-1.18
}

dest {
  import: def
  import: thm
  package: option-dest-1.17
}

main {
  import: def
  import: thm
  import: dest
}
