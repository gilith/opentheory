name: parser-all
version: 1.9
description: Theory of the whole stream parser
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Parser"

def {
  package: parser-all-def-1.7
}

thm {
  import: def
  package: parser-all-thm-1.11
}

main {
  import: def
  import: thm
}
