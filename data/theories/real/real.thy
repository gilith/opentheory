name: real
version: 1.22
description: The real numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural" as "Natural"
show: "Number.Real"
show: "Set"

def {
  package: real-def-1.28
}

thm {
  import: def
  package: real-thm-1.16
}

main {
  import: def
  import: thm
}
