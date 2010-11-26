name: natural-div-mod
version: 1.0
description: Definitions and theorems about natural number division
author: Joe Hurd <joe@gilith.com>
license: OpenTheory
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-div-mod-def-1.0
}

thm {
  import: def
  package: natural-div-mod-thm-1.0
}

main {
  import: def
  import: thm
}
