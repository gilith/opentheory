name: word-bits
version: 1.14
description: Parametric theory of word bit lists
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Word"
show: "Number.Natural" as "Natural"

def {
  package: word-bits-def-1.15
}

thm {
  import: def
  package: word-bits-thm-1.14
}

main {
  import: def
  import: thm
}
