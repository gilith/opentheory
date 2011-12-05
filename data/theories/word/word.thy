name: word
version: 1.33
description: Parametric theory of words
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural
requires: list
requires: natural-divides
show: "Data.Bool"
show: "Data.List"
show: "Data.Word"
show: "Data.Word.Bits"
show: "Number.Natural"

def {
  package: word-def-1.12
}

bits {
  import: def
  package: word-bits-1.29
}

main {
  import: def
  import: bits
}
