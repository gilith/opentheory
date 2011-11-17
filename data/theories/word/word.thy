name: word
version: 1.25
description: Parametric theory of words
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Word"
show: "Number.Natural"

def {
  package: word-def-1.6
}

bits {
  import: def
  package: word-bits-1.23
}

main {
  import: def
  import: bits
}
