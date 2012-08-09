name: option-map
version: 1.2
description: The option map function
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: option-def
requires: option-thm
show: "Data.Bool"
show: "Data.Option"
show: "Function"

def {
  package: option-map-def-1.6
}

thm {
  import: def
  package: option-map-thm-1.6
}

main {
  import: def
  import: thm
}
