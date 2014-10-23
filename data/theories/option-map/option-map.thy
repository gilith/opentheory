name: option-map
version: 1.11
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
  package: option-map-def-1.14
  checksum: 0738d4362c08877c0952eaa46cd8a1816180b852
}

thm {
  import: def
  package: option-map-thm-1.12
  checksum: 49b6ccab38ead93ca3d178ce164d971311e87bfc
}

main {
  import: def
  import: thm
}
