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
  interpret: const "Data.Word.*" as "Data.Byte.*"
  interpret: const "Data.Word.~" as "Data.Byte.~"
  interpret: const "Data.Word.-" as "Data.Byte.-"
  interpret: const "Data.Word.<=" as "Data.Byte.<="
  interpret: const "Data.Word.<" as "Data.Byte.<"
  interpret: const "Data.Word.and" as "Data.Byte.and"
  interpret: const "Data.Word.bit" as "Data.Byte.bit"
  interpret: const "Data.Word.fromList" as "Data.Byte.fromList"
  interpret: const "Data.Word.fromNatural" as "Data.Byte.fromNatural"
  interpret: const "Data.Word.isList" as "Data.Byte.isList"
  interpret: const "Data.Word.not" as "Data.Byte.not"
  interpret: const "Data.Word.or" as "Data.Byte.or"
  interpret: const "Data.Word.shiftLeft" as "Data.Byte.shiftLeft"
  interpret: const "Data.Word.shiftRight" as "Data.Byte.shiftRight"
  interpret: const "Data.Word.size" as "Data.Byte.size"
  interpret: const "Data.Word.toList" as "Data.Byte.toList"
  interpret: const "Data.Word.toNatural" as "Data.Byte.toNatural"
  interpret: const "Data.Word.width" as "Data.Byte.width"
  package: word-1.0
}

main {
  import: def
  import: word
}
