name: bool
version: 1.33
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
  package: axiom-extensionality-1.8
  checksum: a03fa11c985e29adbd28e95405b3fe8c47c0bad5
}

ext {
  import: def
  import: int
  import: axiom-extensionality
  package: bool-ext-1.10
  checksum: 077e7b77524fad6295bd1c0ff445be4274d0826c
}

axiom-choice {
  import: def
  package: axiom-choice-1.7
  checksum: 8b3cfcf9cc420a78142dae227f28d1497cc8e76d
}

class {
  import: def
  import: int
  import: axiom-extensionality
  import: ext
  import: axiom-choice
  package: bool-class-1.23
  checksum: f31ee9a471d9461bfd5bdc3fb399682cf8eb7ebf
}

main {
  import: def
  import: int
  import: axiom-extensionality
  import: ext
  import: axiom-choice
  import: class
}
