name: word16-bytes
version: 1.8
description: Basic theory of 16-bit words as pairs of bytes
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Word16"
show: "Data.List"
show: "Number.Numeral"

def {
  package: word16-bytes-def-1.9
}

thm {
  import: def
  package: word16-bytes-thm-1.10
}

main {
  import: def
  import: thm
}
