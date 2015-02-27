name: parser-all
version: 1.101
description: The whole stream parser
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: parser-comb
requires: parser-stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"

def {
  package: parser-all-def-1.94
}

thm {
  import: def
  package: parser-all-thm-1.103
}

main {
  import: def
  import: thm
}
