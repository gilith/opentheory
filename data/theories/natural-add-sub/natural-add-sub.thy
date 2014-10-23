name: natural-add-sub
version: 1.6
description: Natural number subtraction
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add-def
requires: natural-add-thm
requires: natural-def
requires: natural-dest
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-add-sub-def-1.8
  checksum: 1a87a31c9903a66831e27aa64e3964d1931da34e
}

thm {
  import: def
  package: natural-add-sub-thm-1.6
  checksum: 4835aa0d6bc989d54042f9ceba3c06577aa3738e
}

main {
  import: def
  import: thm
}
