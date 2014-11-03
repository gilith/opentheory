name: stream
version: 1.37
description: Stream types
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: pair
requires: set
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"
show: "Set"

def {
  package: stream-def-1.33
  checksum: 43289b6f1da7f334e42ecee31bce1daac5a7dbf4
}

thm {
  import: def
  package: stream-thm-1.35
  checksum: 8ae9206dc03cf4df0eb9f707145f46d8ba7cb00d
}

main {
  import: def
  import: thm
}
