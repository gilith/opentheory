name: list-length
version: 1.50
description: The list length function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-def
requires: list-dest
requires: list-thm
requires: natural
show: "Data.Bool"
show: "Data.List"
show: "Number.Natural"

def {
  package: list-length-def-1.46
  checksum: 2532c21dea2e736700b5ffe073d52d35612e5462
}

thm {
  import: def
  package: list-length-thm-1.41
  checksum: 2e5d3db54c129b71d861593d84e9399acda026e0
}

main {
  import: def
  import: thm
}
