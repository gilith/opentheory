name: real
version: 1.5
description: The real numbers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural" as "Natural"
show: "Number.Numeral"
show: "Number.Real"
show: "Set"

def {
  package: real-def-1.9
}

thm {
  import: def
  package: real-thm-1.4
}

main {
  import: def
  import: thm
}
