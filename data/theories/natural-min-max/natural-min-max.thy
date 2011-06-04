name: natural-min-max
version: 1.1
description: Natural number min and max functions
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: natural-min-max-1.0
}

thm {
  import: def
  package: natural-min-max-1.0
}

main {
  import: def
  import: thm
}
