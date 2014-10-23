name: list-take-drop
version: 1.59
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
  package: list-take-drop-def-1.53
  checksum: 42e0464c0677717760dc2859c32f0e99c9f32914
}

thm {
  import: def
  package: list-take-drop-thm-1.60
  checksum: 7e69792efb4ae207d706a5356b7a2e31113522ea
}

main {
  import: def
  import: thm
}
