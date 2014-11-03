name: option-map
version: 1.13
description: The option map function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: option-def
requires: option-thm
show: "Data.Bool"
show: "Data.Option"
show: "Function"

def {
  package: option-map-def-1.16
  checksum: 3f119d70a0cdf29f0f5be67a44b3683fd9cbf7e2
}

thm {
  import: def
  package: option-map-thm-1.14
  checksum: 7622c09c1033bee20370b9a954302612ad1ddc74
}

main {
  import: def
  import: thm
}
