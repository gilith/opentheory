name: examples
version: 1.41
description: All the example theories
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

byte {
  package: byte-1.21
}

word10 {
  package: word10-1.20
}

word12 {
  package: word12-1.19
}

word16 {
  import: byte
  package: word16-1.25
}

parser {
  package: parser-1.28
}

char {
  import: byte
  import: word16
  import: parser
  package: char-1.31
}

h {
  import: byte
  import: word10
  import: word12
  package: h-1.28
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
