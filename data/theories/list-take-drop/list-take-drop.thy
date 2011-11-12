name: list-take-drop
version: 1.18
description: Definitions and theorems about the list take and drop functions
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-take-drop-def-1.18
}

thm {
  import: def
  package: list-take-drop-thm-1.18
}

main {
  import: def
  import: thm
}
