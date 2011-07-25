name: option
version: 1.15
description: Basic theory of option types
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Option"
show: "Number.Natural"

def {
  package: option-def-1.9
}

thm {
  import: def
  package: option-thm-1.8
}

dest {
  import: def
  import: thm
  package: option-dest-1.13
}

main {
  import: def
  import: thm
  import: dest
}
