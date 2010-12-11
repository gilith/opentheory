name: char
version: 1.0
description: Theory of Unicode characters
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Data.Char"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: char-def-1.0
}

plane {
  import: def
  interpret: type "Number.Modular.modular" as "Data.Char.Plane.plane"
  interpret: const "Number.Modular.fromNatural" as "Data.Char.Plane.fromNatural"
  interpret: const "Number.Modular.toNatural" as "Data.Char.Plane.toNatural"
  interpret: const "Number.Modular.size" as "Data.Char.Plane.size"
  package: modular-1.0
}

main {
  import: def
  import: plane
}
