name: examples
version: 1.9
description: All the example theories
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

byte {
  package: byte-1.1
}

word16 {
  import: byte
  package: word16-1.2
}

parser {
  package: parser-1.5
}

char {
  import: byte
  import: word16
  import: parser
  package: char-1.9
}

main {
  import: byte
  import: word16
  import: parser
  import: char
}
