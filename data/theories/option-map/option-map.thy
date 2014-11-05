name: option-map
version: 1.14
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
  package: option-map-def-1.17
  checksum: ca8ea042fa4ca486f389b79b2ba49e95409989b8
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
