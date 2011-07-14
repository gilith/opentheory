name: examples
version: 1.22
description: All the example theories
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

byte {
  package: byte-1.9
}

word10 {
  package: word10-1.8
}

word12 {
  package: word12-1.7
}

word16 {
  import: byte
  package: word16-1.11
}

parser {
  package: parser-1.16
}

char {
  import: byte
  import: word16
  import: parser
  package: char-1.18
}

h {
  import: byte
  import: word10
  import: word12
  package: h-1.11
}

main {
  import: byte
  import: word10
  import: word12
  import: word16
  import: parser
  import: char
  import: h
}
