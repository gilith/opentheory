name: word
version: 1.48
description: Parametric theory of words
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: natural-divides
show: "Data.Bool"
show: "Data.List"
show: "Data.Word"
show: "Data.Word.Bits"
show: "Number.Natural"

def {
  package: word-def-1.27
}

bits {
  import: def
  package: word-bits-1.44
}

main {
  import: def
  import: bits
}
