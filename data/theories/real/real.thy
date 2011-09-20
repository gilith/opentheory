name: real
version: 1.9
description: The real numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural" as "Natural"
show: "Number.Real"
show: "Set"

def {
  package: real-def-1.13
}

thm {
  import: def
  package: real-thm-1.6
}

main {
  import: def
  import: thm
}
