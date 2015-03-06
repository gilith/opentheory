name: parser-fold
version: 1.1
description: The fold parsers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: base
requires: parser-comb
requires: parser-stream
show: "Data.Bool"
show: "Data.Option"
show: "Data.Pair"
show: "Data.Sum"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"

def {
  package: parser-fold-def-1.1
}

thm {
  import: def
  package: parser-fold-thm-1.2
}

main {
  import: def
  import: thm
}
