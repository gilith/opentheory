name: word-bits
version: 1.15
description: Parametric theory of word bit lists
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Word"
show: "Number.Natural" as "Natural"

def {
  package: word-bits-def-1.16
}

thm {
  import: def
  package: word-bits-thm-1.15
}

main {
  import: def
  import: thm
}
