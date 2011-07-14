name: word16-bytes
version: 1.10
description: Basic theory of 16-bit words as pairs of bytes
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Word16"
show: "Data.List"
show: "Number.Numeral"

def {
  package: word16-bytes-def-1.11
}

thm {
  import: def
  package: word16-bytes-thm-1.12
}

main {
  import: def
  import: thm
}
