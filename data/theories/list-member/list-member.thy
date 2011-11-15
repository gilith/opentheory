name: list-member
version: 1.23
description: Definitions and theorems about the list member function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-member-def-1.17
}

thm {
  import: def
  package: list-member-thm-1.25
}

main {
  import: def
  import: thm
}
