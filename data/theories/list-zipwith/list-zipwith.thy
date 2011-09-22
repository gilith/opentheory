name: list-zipwith
version: 1.13
description: Definitions and theorems about the list zipWith function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"

def {
  package: list-zipwith-def-1.14
}

thm {
  import: def
  package: list-zipwith-thm-1.10
}

main {
  import: def
  import: thm
}
