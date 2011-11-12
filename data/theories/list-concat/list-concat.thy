name: list-concat
version: 1.14
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
  package: list-concat-thm-1.4
}

main {
  import: def
  import: thm
}
