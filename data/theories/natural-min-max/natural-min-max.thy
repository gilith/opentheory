name: natural-min-max
version: 1.6
description: Natural number min and max functions
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-min-max-def-1.3
}

thm {
  import: def
  package: natural-min-max-thm-1.3
}

main {
  import: def
  import: thm
}
