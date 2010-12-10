name: byte
version: 1.0
description: Basic theory of bytes
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Data.Byte"
show: "Number.Numeral"

def {
  package: byte-def-1.0
}

word {
  import: def
  interpret: type "Data.Word.word" as "Data.Byte.byte"
  interpret: const "Data.Word.fromNatural" as "Data.Byte.fromNatural"
  interpret: const "Data.Word.toNatural" as "Data.Byte.toNatural"
  interpret: const "Data.Word.size" as "Data.Byte.size"
  package: word-1.0
}

main {
  import: def
  import: word
}
