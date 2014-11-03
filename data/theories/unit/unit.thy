name: unit
version: 1.20
description: The unit type
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
show: "Data.Bool"
show: "Data.Unit"

def {
  package: unit-def-1.13
  checksum: 5f6feb4c479325bbb29dd41bcf726dc252ee0910
}

thm {
  import: def
  package: unit-thm-1.16
  checksum: 88c4527cf282b08a176d6b9d9444f91a09053c20
}

main {
  import: def
  import: thm
}
