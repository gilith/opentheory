name: list-take-drop
version: 1.61
description: The list take and drop functions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-dest
requires: list-length
requires: list-nth
requires: list-replicate
requires: list-thm
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-take-drop-def-1.55
  checksum: d25667297140a3969da7fb1e2c9440270e40707a
}

thm {
  import: def
  package: list-take-drop-thm-1.62
  checksum: e8f40b8b90aa0aad3c27a05242dd856cdfc52894
}

main {
  import: def
  import: thm
}
