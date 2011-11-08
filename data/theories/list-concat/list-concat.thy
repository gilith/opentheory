name: list-concat
version: 1.13
description: Definitions and theorems about the list concat function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-concat-def-1.15
}

thm {
  import: def
  package: list-concat-thm-1.3
}

main {
  import: def
  import: thm
}
