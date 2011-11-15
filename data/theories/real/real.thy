name: real
version: 1.21
description: The real numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural" as "Natural"
show: "Number.Real"
show: "Set"

def {
  package: real-def-1.27
}

thm {
  import: def
  package: real-thm-1.15
}

main {
  import: def
  import: thm
}
