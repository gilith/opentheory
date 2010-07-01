name: hol-light-class
version: 2009.8.24
description: HOL Light class theory
author: Joe Hurd <joe@gilith.com>
license: HOLLight

class-eta {
  package: hol-light-class-eta-2009.8.24
}

class-select {
  import: class-eta
  package: hol-light-class-select-2009.8.24
}

class-cond {
  import: class-eta
  import: class-select
  package: hol-light-class-cond-2009.8.24
}

class-skolem {
  import: class-eta
  import: class-select
  import: class-cond
  package: hol-light-class-skolem-2009.8.24
}

class-bool {
  import: class-eta
  import: class-select
  import: class-cond
  import: class-skolem
  package: hol-light-class-bool-2009.8.24
}

main {
  import: class-eta
  import: class-select
  import: class-cond
  import: class-skolem
  import: class-bool
}
