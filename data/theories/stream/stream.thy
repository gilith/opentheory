name: stream
version: 1.6
description: Stream types
author: Joe Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: list
requires: natural
requires: pair
show: "Data.Bool"
show: "Data.List"
show: "Data.Pair"
show: "Data.Stream"
show: "Number.Natural"

def {
  package: stream-def-1.10
}

thm {
  import: def
  package: stream-thm-1.7
}

main {
  import: def
  import: thm
}
