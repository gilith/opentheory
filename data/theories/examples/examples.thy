name: examples
version: 1.44
description: All the example theories
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

byte {
  package: byte-1.23
}

word10 {
  package: word10-1.22
}

word12 {
  package: word12-1.21
}

word16 {
  import: byte
  package: word16-1.27
}

parser {
  package: parser-1.31
}

char {
  import: byte
  import: word16
  import: parser
  package: char-1.34
}

h {
  import: byte
  import: word10
  import: word12
  package: h-1.30
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
