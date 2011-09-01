name: base-examples
version: 1.32
description: All the example theories built on top of the standard library
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

base {
  package: base-1.23
}

examples {
  import: base
  package: examples-1.26
}

main {
  import: examples
}
