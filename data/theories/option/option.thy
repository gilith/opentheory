name: option
version: 1.39
description: Option types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
show: "Data.Bool"
show: "Data.Option"
show: "Number.Natural"

def {
  package: option-def-1.35
}

thm {
  import: def
  package: option-thm-1.28
}

dest {
  import: def
  import: thm
  package: option-dest-1.29
}

main {
  import: def
  import: thm
  import: dest
}
