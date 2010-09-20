name: bool-true
version: 1.0
description: Definition and theorems about the boolean true constant
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"

bool-def-true {
  package: bool-def-true-1.0
}

bool-int-true {
  import: bool-def-true
  package: bool-int-true-1.0
}

main {
  import: bool-def-true
  import: bool-int-true
}
