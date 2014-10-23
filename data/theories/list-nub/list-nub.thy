name: list-nub
version: 1.54
description: The list nub function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-length
requires: list-reverse
requires: list-set
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-nub-def-1.51
  checksum: 96911d5c53607208a6cead6c892040b6b88e151b
}

thm {
  import: def
  package: list-nub-thm-1.55
  checksum: c3db086ed6c24be477d609202daa0f165b784dba
}

main {
  import: def
  import: thm
}
