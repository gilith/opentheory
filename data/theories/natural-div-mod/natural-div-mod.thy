name: natural-div-mod
version: 1.7
description: Definitions and theorems about natural number division
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-div-mod-def-1.7
}

thm {
  import: def
  package: natural-div-mod-thm-1.6
}

main {
  import: def
  import: thm
}
