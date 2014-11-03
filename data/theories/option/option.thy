name: option
version: 1.69
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
  package: option-def-1.60
  checksum: d5fca5149fe579f45e0aa8f3ee9a5ba0e202ab91
}

thm {
  import: def
  package: option-thm-1.53
  checksum: 0269331160032c8448335045747d281396643630
}

dest {
  import: def
  import: thm
  package: option-dest-1.54
  checksum: 1b1a73449ded92eb0052d86e299e71a6d423eb40
}

map {
  import: def
  import: thm
  package: option-map-1.13
  checksum: b344bb8e3fd2fa7b68019ebcdbc5052b688311d7
}

main {
  import: def
  import: thm
  import: dest
  import: map
}
