name: natural-div-mod
version: 1.6
description: Definitions and theorems about natural number division
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-div-mod-def-1.6
}

thm {
  import: def
  package: natural-div-mod-thm-1.5
}

main {
  import: def
  import: thm
}
