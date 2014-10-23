name: list-append
version: 1.55
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
  package: list-append-def-1.48
  checksum: 5b7605a2c58ce143eb581f9f1c79560ecc6acb52
}

thm {
  import: def
  package: list-append-thm-1.27
  checksum: 6440dfbad1beba13edccd9b13d0846f2dd010aa1
}

main {
  import: def
  import: thm
}
