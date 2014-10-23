name: parser-all
version: 1.89
description: The whole stream parser
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: option
requires: pair
requires: parser-comb
requires: parser-stream
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"

def {
  package: parser-all-def-1.87
  checksum: a977d2d1283e0e6e99a0970c493e7af4379b7b8e
}

thm {
  import: def
  package: parser-all-thm-1.93
  checksum: e440b35b46924ac707eb0deb6d0a2912bdae197b
}

main {
  import: def
  import: thm
}
