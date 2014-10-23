name: natural-factorial
version: 1.35
description: Natural number factorial
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: natural-add
requires: natural-def
requires: natural-mult
requires: natural-numeral
requires: natural-order
requires: natural-thm
show: "Data.Bool"
show: "Number.Natural"

def {
  package: natural-factorial-def-1.32
  checksum: 0ade9f765e64529f247f93a73690e055243bf9cd
}

thm {
  import: def
  package: natural-factorial-thm-1.32
  checksum: 570d208e7833d2876d47a1c07c12086372301eb7
}

main {
  import: def
  import: thm
}
