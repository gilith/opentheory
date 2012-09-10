name: stream
version: 1.17
description: Stream types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: pair
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Function"
show: "Number.Natural"

def {
  package: stream-def-1.20
}

thm {
  import: def
  package: stream-thm-1.17
}

main {
  import: def
  import: thm
}
