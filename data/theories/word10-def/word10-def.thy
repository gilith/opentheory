name: word10-def
version: 1.98
description: Definition of 10-bit words
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2011-07-25
requires: base
requires: natural-bits
requires: natural-divides
requires: probability
show: "Data.Bool"
show: "Data.List"
show: "Data.Word10"
show: "Data.Word10.Bits"
show: "Number.Natural"
show: "Probability.Random"

def {
  article: "word10-def.art"
}

word {
  import: def
  interpretation: "word.int"
  package: word-1.121
}

main {
  import: def
  import: word
}
