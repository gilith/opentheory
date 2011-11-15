name: word16-bytes
version: 1.21
description: Theory of 16-bit words as pairs of bytes
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Byte" as "Byte"
show: "Data.List"
show: "Data.Pair"
show: "Data.Word16"
show: "Number.Natural" as "Natural"

def {
  package: word16-bytes-def-1.19
}

thm {
  import: def
  package: word16-bytes-thm-1.23
}

main {
  import: def
  import: thm
}
