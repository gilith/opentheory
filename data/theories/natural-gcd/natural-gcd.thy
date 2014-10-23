name: natural-gcd
version: 1.48
description: Natural number greatest common divisor
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: natural-divides
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-gcd-def-1.38
  checksum: 996cbb884593caf13349922d17244b3cf20993cd
}

thm {
  import: def
  package: natural-gcd-thm-1.46
  checksum: cef967c43b98ae8206b47dbcaef90f81a8c4c3ae
}

lcm {
  import: def
  import: thm
  package: natural-gcd-lcm-1.36
  checksum: b6f990efd28abeb362cff3838c138ff79568090a
}

main {
  import: def
  import: thm
  import: lcm
}
