name: natural-div-mod
version: 1.1
description: Definitions and theorems about natural number division
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-div-mod-def-1.1
}

thm {
  import: def
  package: natural-div-mod-thm-1.1
}

main {
  import: def
  import: thm
}
