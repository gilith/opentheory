name: h
version: 1.13
description: The memory safety proof of the H API
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.Byte"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Word10"
show: "Data.Word12"
show: "Function"
show: "Number.Natural"
show: "Number.Numeral"
show: "Set"
show: "System.H"

def {
  package: h-def-1.17
}

thm {
  import: def
  package: h-thm-1.17
}

main {
  import: def
  import: thm
}
