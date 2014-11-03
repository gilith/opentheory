name: natural-gcd-lcm
version: 1.38
description: Natural number least common multiple
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: natural-divides
requires: natural-gcd-def
requires: natural-gcd-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-gcd-lcm-def-1.34
  checksum: bded699af99e34a9ace0c3aecab4092792a33b00
}

thm {
  import: def
  package: natural-gcd-lcm-thm-1.39
  checksum: f63c5fc8ba2620de5c4f67a7c761ef07ca68bca3
}

main {
  import: def
  import: thm
}
