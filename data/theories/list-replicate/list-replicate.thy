name: list-replicate
version: 1.62
description: The list replicate function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-length
requires: list-map
requires: list-nth
requires: list-set
requires: list-thm
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-replicate-def-1.53
  checksum: ff13a038473f08517db64148ff98d1e34b8bfe02
}

thm {
  import: def
  package: list-replicate-thm-1.61
  checksum: f16767711414ce88b2ac2439eeefb31637810f30
}

main {
  import: def
  import: thm
}
