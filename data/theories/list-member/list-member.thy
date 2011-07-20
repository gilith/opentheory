name: list-member
version: 1.12
description: Definitions and theorems about the list member function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: list-member-def-1.11
}

thm {
  import: def
  package: list-member-thm-1.12
}

main {
  import: def
  import: thm
}
