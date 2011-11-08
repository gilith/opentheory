name: base-examples
version: 1.51
description: All the example theories built on top of the standard library
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

base {
  package: base-1.40
}

examples {
  import: base
  package: examples-1.34
}

main {
  import: examples
}
