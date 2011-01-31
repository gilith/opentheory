name: parser-basic
version: 1.0
description: Basic theory of parser combinators
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Parser"

def {
  package: parser-basic-def-1.0
}

thm {
  import: def
  package: parser-basic-thm-1.0
}

main {
  import: def
  import: thm
}
