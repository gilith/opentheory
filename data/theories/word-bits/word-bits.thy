name: word-bits
version: 1.20
description: Parametric theory of word bit lists
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Word"
show: "Number.Natural" as "Natural"

def {
  package: word-bits-def-1.19
}

thm {
  import: def
  package: word-bits-thm-1.20
}

main {
  import: def
  import: thm
}
