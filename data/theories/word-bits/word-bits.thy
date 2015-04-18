name: word-bits
version: 1.100
description: Word to bit-list conversions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: natural-bits
requires: word-def
show: "Data.Bool"
show: "Data.List"
show: "Data.Word"
show: "Data.Word.Bits"
show: "Number.Natural"

def {
  package: word-bits-def-1.85
}

thm {
  import: def
  package: word-bits-thm-1.100
}

main {
  import: def
  import: thm
}
