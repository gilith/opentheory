name: option
version: 1.67
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
  package: option-def-1.58
  checksum: 65303c5e2782706be4c664a1e2c901afde9e9ca6
}

thm {
  import: def
  package: option-thm-1.51
  checksum: 5846bd15762a08d88546800d407f361a56229b04
}

dest {
  import: def
  import: thm
  package: option-dest-1.52
  checksum: a85283adec43c99c0f145f72a0dfe31ef2042ecb
}

map {
  import: def
  import: thm
  package: option-map-1.11
  checksum: 525f3a97f7defc44eff80f45ffca7d05be36650a
}

main {
  import: def
  import: thm
  import: dest
  import: map
}
