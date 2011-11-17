name: word-def
version: 1.6
description: word-def
author: Joe Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2011-11-12
requires: bool
requires: natural
show: "Data.Bool"
show: "Data.Word"
show: "Number.Natural"

def {
  article: "word-def.art"
}

modular {
  import: def
  interpret: type "Number.Modular.modular" as "Data.Word.word"
  interpret: const "Number.Modular.*" as "Data.Word.*"
  interpret: const "Number.Modular.+" as "Data.Word.+"
  interpret: const "Number.Modular.-" as "Data.Word.-"
  interpret: const "Number.Modular.<" as "Data.Word.<"
  interpret: const "Number.Modular.<=" as "Data.Word.<="
  interpret: const "Number.Modular.~" as "Data.Word.~"
  interpret: const "Number.Modular.fromNatural" as "Data.Word.fromNatural"
  interpret: const "Number.Modular.modulus" as "Data.Word.modulus"
  interpret: const "Number.Modular.toNatural" as "Data.Word.toNatural"
  package: modular-1.23
}

main {
  import: def
  import: modular
}
