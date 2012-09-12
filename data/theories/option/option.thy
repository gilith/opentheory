name: option
version: 1.57
description: Option types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
show: "Data.Bool"
show: "Data.Option"
show: "Function"
show: "Number.Natural"

def {
  package: option-def-1.52
}

thm {
  import: def
  package: option-thm-1.44
}

dest {
  import: def
  import: thm
  package: option-dest-1.44
}

map {
  import: def
  import: thm
  package: option-map-1.3
}

main {
  import: def
  import: thm
  import: dest
  import: map
}
