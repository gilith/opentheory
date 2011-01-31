name: byte
version: 1.0
description: Basic theory of bytes
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Byte"
show: "Data.List"
show: "Number.Numeral"

def {
  package: byte-def-1.0
}

word {
  import: def
  interpret: type "Data.Word.word" as "Data.Byte.byte"
  interpret: const "Data.Word.+" as "Data.Byte.+"
  interpret: const "Data.Word.fromNatural" as "Data.Byte.fromNatural"
  interpret: const "Data.Word.size" as "Data.Byte.size"
  interpret: const "Data.Word.toList" as "Data.Byte.toList"
  interpret: const "Data.Word.toList.f" as "Data.Byte.toList.f"
  interpret: const "Data.Word.toNatural" as "Data.Byte.toNatural"
  interpret: const "Data.Word.width" as "Data.Byte.width"
  package: word-1.0
}

main {
  import: def
  import: word
}
