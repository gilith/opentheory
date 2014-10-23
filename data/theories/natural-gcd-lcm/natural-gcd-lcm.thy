name: natural-gcd-lcm
version: 1.36
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
  package: natural-gcd-lcm-def-1.32
  checksum: 5569211f3bece471c3ea7609b429b4282faae07d
}

thm {
  import: def
  package: natural-gcd-lcm-thm-1.37
  checksum: bd21d6be622919f0b13c238f67f564e8aea4d126
}

main {
  import: def
  import: thm
}
