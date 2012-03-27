name: word-bits
version: 1.44
description: Word to bit-list conversions
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: word-def
show: "Data.Bool"
show: "Data.List"
show: "Data.Word"
show: "Data.Word.Bits"
show: "Number.Natural"

def {
  package: word-bits-def-1.43
}

thm {
  import: def
  package: word-bits-thm-1.48
}

main {
  import: def
  import: thm
}
