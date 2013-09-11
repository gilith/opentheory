name: montgomery
version: 1.9
description: Montgomery multiplication
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
provenance: HOL Light theory extracted on 2012-11-09
requires: bool
requires: natural
show: "Data.Bool"
show: "Number.Natural"

def {
  package: montgomery-def-1.5
}

thm {
  import: def
  package: montgomery-thm-1.10
}

main {
  import: def
  import: thm
}
