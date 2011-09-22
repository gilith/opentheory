name: natural-min-max
version: 1.8
description: Natural number min and max functions
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-min-max-def-1.4
}

thm {
  import: def
  package: natural-min-max-thm-1.5
}

main {
  import: def
  import: thm
}
