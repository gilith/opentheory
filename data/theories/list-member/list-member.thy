name: list-member
version: 1.0
description: Definitions and theorems about the list member function
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Number.Numeral"

def {
  package: list-member-def-1.0
}

thm {
  import: def
  package: list-member-thm-1.0
}

main {
  import: def
  import: thm
}
