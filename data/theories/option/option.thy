name: option
version: 1.37
description: Option types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
show: "Data.Bool"
show: "Data.Option"
show: "Number.Natural"

def {
  package: option-def-1.33
}

thm {
  import: def
  package: option-thm-1.27
}

dest {
  import: def
  import: thm
  package: option-dest-1.28
}

main {
  import: def
  import: thm
  import: dest
}
