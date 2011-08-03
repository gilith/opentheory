name: real
version: 1.8
description: The real numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural" as "Natural"
show: "Number.Real"
show: "Set"

def {
  package: real-def-1.12
}

thm {
  import: def
  package: real-thm-1.5
}

main {
  import: def
  import: thm
}
