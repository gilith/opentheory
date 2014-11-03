name: parser
version: 1.105
description: Stream parsers
author: Joe Leslie-Hurd <joe@gilith.com>
license: MIT
requires: bool
requires: function
requires: list
requires: natural
requires: option
requires: pair
requires: probability
requires: relation
show: "Data.Bool"
show: "Data.List"
show: "Data.Option"
show: "Data.Pair"
show: "Function"
show: "Number.Natural"
show: "Parser"
show: "Parser.Stream"
show: "Probability.Random"
show: "Relation"

stream {
  package: parser-stream-1.96
  checksum: 68fca2ff2b91cfb2ca43bebbfc1b810f1023bbf3
}

comb {
  import: stream
  package: parser-comb-1.87
  checksum: 47cc78fddb53943a010f4f65f4a1e8b685975673
}

all {
  import: stream
  import: comb
  package: parser-all-1.91
  checksum: bf5b23f9047646657500022bb0c522cc1c170414
}

main {
  import: stream
  import: comb
  import: all
}
