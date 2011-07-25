name: natural-div-mod
version: 1.4
description: Definitions and theorems about natural number division
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-div-mod-def-1.4
}

thm {
  import: def
  package: natural-div-mod-thm-1.3
}

main {
  import: def
  import: thm
}
