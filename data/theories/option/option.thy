name: option
version: 1.70
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
  package: option-def-1.61
  checksum: 01e7e75e16564c5a3a32d891fc6e5f80893b4eb9
}

thm {
  import: def
  package: option-thm-1.53
  checksum: 0269331160032c8448335045747d281396643630
}

dest {
  import: def
  import: thm
  package: option-dest-1.55
  checksum: 21433634bc61793fff3366a1331ccf656379d259
}

map {
  import: def
  import: thm
  package: option-map-1.14
  checksum: 8d0c71b57363fe40b2f39dd3c3a510f1f9421c07
}

main {
  import: def
  import: thm
  import: dest
  import: map
}
