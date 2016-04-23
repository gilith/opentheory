name: byte-def
version: 1.98
description: Definition of bytes
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2011-07-25
requires: base
requires: natural-bits
requires: natural-divides
requires: probability
show: "Data.Bool"
show: "Data.Byte"
show: "Data.Byte.Bits"
show: "Data.List"
show: "Number.Natural"
show: "Probability.Random"

def {
  article: "byte-def.art"
}

word {
  import: def
  interpretation: "word.int"
  package: word-1.120
}

main {
  import: def
  import: word
}
