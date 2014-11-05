name: list-zip
version: 1.27
description: The list zip function
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list-append
requires: list-def
requires: list-dest
requires: list-length
requires: list-nth
requires: natural
requires: pair
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Number.Natural"

def {
  package: list-zip-def-1.21
  checksum: 9bb1de13519908519e77c1927557860689c44294
}

thm {
  import: def
  package: list-zip-thm-1.24
  checksum: 3ddb3d183f8b466e3e5ddc8b3bbd1db16254a73a
}

main {
  import: def
  import: thm
}
