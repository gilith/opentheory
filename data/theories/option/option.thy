name: option
version: 1.44
description: Option types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
show: "Data.Bool"
show: "Data.Option"
show: "Number.Natural"

def {
  package: option-def-1.40
}

thm {
  import: def
  package: option-thm-1.31
}

dest {
  import: def
  import: thm
  package: option-dest-1.33
}

main {
  import: def
  import: thm
  import: dest
}
