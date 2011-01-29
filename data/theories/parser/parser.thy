name: parser
version: 1.0
description: Basic theory of parsers
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Parser"

def {
  package: parser-def-1.0
}

thm {
  import: def
  package: parser-thm-1.0
}

main {
  import: def
  import: thm
}
