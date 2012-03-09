name: option
version: 1.43
description: Option types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
show: "Data.Bool"
show: "Data.Option"
show: "Number.Natural"

def {
  package: option-def-1.39
}

thm {
  import: def
  package: option-thm-1.30
}

dest {
  import: def
  import: thm
  package: option-dest-1.32
}

main {
  import: def
  import: thm
  import: dest
}
