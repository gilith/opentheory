name: bool-true
version: 1.0
description: Definition and theorems about the boolean true constant
author: Joe Hurd <joe@gilith.com>
license: PublicDomain
show: "Data.Bool"

bool-true-def {
  package: bool-true-def-1.0
}

bool-true-thm {
  import: bool-true-def
  package: bool-true-thm-1.0
}

main {
  import: bool-true-def
  import: bool-true-thm
}
