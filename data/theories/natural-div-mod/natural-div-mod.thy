name: natural-div-mod
version: 1.11
description: Natural number division
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-div-mod-def-1.10
}

thm {
  import: def
  package: natural-div-mod-thm-1.11
}

main {
  import: def
  import: thm
}
