name: option
version: 1.64
description: Option types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: natural
show: "Data.Bool"
show: "Data.Option"
show: "Function"
show: "Number.Natural"

def {
  package: option-def-1.57
}

thm {
  import: def
  package: option-thm-1.49
}

dest {
  import: def
  import: thm
  package: option-dest-1.49
}

map {
  import: def
  import: thm
  package: option-map-1.8
}

main {
  import: def
  import: thm
  import: dest
  import: map
}
