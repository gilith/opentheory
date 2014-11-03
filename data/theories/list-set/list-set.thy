name: list-set
version: 1.53
description: List to set conversions
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
requires: list-length
requires: natural
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"
show: "Set"

def {
  package: list-set-def-1.52
  checksum: 35a44f8ef26cd30a43629e8502af91924f28dccd
}

thm {
  import: def
  package: list-set-thm-1.50
  checksum: 79b0a5fbf1423de0767387fbbe042b15c1742415
}

main {
  import: def
  import: thm
}
