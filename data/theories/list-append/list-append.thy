name: list-append
version: 1.57
description: Appending lists
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
requires: list-length
requires: list-set
requires: list-thm
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-append-def-1.50
  checksum: f062c321541e3f5b4d5ebd13b13611af0970e31a
}

thm {
  import: def
  package: list-append-thm-1.29
  checksum: 6a9f944ed94735749633b1aeedb3cb5965bfea03
}

main {
  import: def
  import: thm
}
