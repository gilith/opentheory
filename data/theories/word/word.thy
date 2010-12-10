name: word
version: 1.0
description: Basic functor theory of words
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"
show: "Data.Word"
show: "Number.Numeral"

def {
  package: word-def-1.0
}

modular {
  import: def
  interpret: type "Number.Modular.modular" as "Data.Word.word"
  interpret: const "Number.Modular.fromNatural" as "Data.Word.fromNatural"
  interpret: const "Number.Modular.toNatural" as "Data.Word.toNatural"
  interpret: const "Number.Modular.size" as "Data.Word.size"
  package: modular-1.0
}

main {
  import: def
  import: modular
}
