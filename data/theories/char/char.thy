name: char
version: 1.0
description: Theory of Unicode characters
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Char"
show: "Data.List"
show: "Number.Natural"
show: "Number.Numeral"

plane-def {
  package: char-plane-def-1.0
}

plane-modular {
  import: plane-def
  interpret: type "Number.Modular.modular" as "Data.Char.Plane.plane"
  interpret: const "Number.Modular.+" as "Data.Char.Plane.+"
  interpret: const "Number.Modular.fromNatural" as "Data.Char.Plane.fromNatural"
  interpret: const "Number.Modular.toNatural" as "Data.Char.Plane.toNatural"
  interpret: const "Number.Modular.size" as "Data.Char.Plane.size"
  package: modular-1.0
}

plane {
  import: plane-def
  import: plane-modular
}

position-def {
  package: char-position-def-1.0
}

position-word {
  import: position-def
  interpret: type "Data.Word.word" as "Data.Char.Position.position"
  interpret: const "Data.Word.+" as "Data.Char.Position.+"
  interpret: const "Data.Word.fromNatural" as "Data.Char.Position.fromNatural"
  interpret: const "Data.Word.size" as "Data.Char.Position.size"
  interpret: const "Data.Word.toList" as "Data.Char.Position.toList"
  interpret: const "Data.Word.toList.f" as "Data.Char.Position.toList.f"
  interpret: const "Data.Word.toNatural" as "Data.Char.Position.toNatural"
  interpret: const "Data.Word.width" as "Data.Char.Position.width"
  package: word-1.0
}

position {
  import: position-def
  import: position-word
}

def {
  import: plane
  import: position
  package: char-def-1.0
}

main {
  import: plane
  import: position
  import: def
}
