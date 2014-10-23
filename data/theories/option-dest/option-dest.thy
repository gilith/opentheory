name: option-dest
version: 1.52
description: Option type destructors
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: option-def
requires: option-thm
show: "Data.Bool"
show: "Data.Option"

def {
  package: option-dest-def-1.57
  checksum: c0fc3e5e42e62a453a1e6eb975e98cd4f30eba0e
}

thm {
  import: def
  package: option-dest-thm-1.13
  checksum: a0fd660f19b80baa3e6eca92e382ba62b6cdd79e
}

main {
  import: def
  import: thm
}
