name: base-examples
version: 1.13
description: All the example theories built on top of the standard library
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

base {
  package: base-1.3
}

examples {
  import: base
  package: examples-1.12
}

main {
  import: examples
}
