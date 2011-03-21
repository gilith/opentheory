name: base-examples
version: 1.10
description: All the example theories built on top of the standard library
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

base {
  package: base-1.2
}

examples {
  import: base
  package: examples-1.9
}

main {
  import: examples
}
