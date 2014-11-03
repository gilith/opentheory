name: bool
version: 1.35
description: Boolean operators and quantifiers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

def {
  package: bool-def-1.10
  checksum: 1eeee88a839070b8f3f6adfbdeb61911bd140360
}

int {
  import: def
  package: bool-int-1.17
  checksum: e69128a68b5292fcf37afb04eba4ed2eb39021a7
}

axiom-extensionality {
  import: def
  package: axiom-extensionality-1.9
  checksum: 6e9a7c8e10c21e54d5f3285cb11e65de99c9542d
}

ext {
  import: def
  import: int
  import: axiom-extensionality
  package: bool-ext-1.12
  checksum: cb4c404d1f0ba4bf11042416a15861eebc4e45cf
}

axiom-choice {
  import: def
  package: axiom-choice-1.8
  checksum: 2178247da99e65a9a2e0a1093adbff512d1539d9
}

class {
  import: def
  import: int
  import: axiom-extensionality
  import: ext
  import: axiom-choice
  package: bool-class-1.25
  checksum: 67a0d94c7ba1b96c98f5775db1d69fc1a3581651
}

main {
  import: def
  import: int
  import: axiom-extensionality
  import: ext
  import: axiom-choice
  import: class
}
