name: relation-measure
version: 1.10
description: Definitions and theorems about natural number measures
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Number.Natural"
show: "Relation"

def {
  package: relation-measure-def-1.10
}

thm {
  import: def
  package: relation-measure-thm-1.11
}

main {
  import: def
  import: thm
}
