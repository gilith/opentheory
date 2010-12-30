name: bool-def
version: 1.0
description: Basic boolean definitions
author: Joe Hurd <joe@gilith.com>
license: MIT
show: "Data.Bool"

bool-def-true {
  package: bool-def-true-1.0
}

bool-def-and {
  import: bool-def-true
  package: bool-def-and-1.0
}

bool-def-imp {
  import: bool-def-and
  package: bool-def-imp-1.0
}

bool-def-forall {
  import: bool-def-true
  package: bool-def-forall-1.0
}

bool-def-exists {
  import: bool-def-imp
  import: bool-def-forall
  package: bool-def-exists-1.0
}

bool-def-or {
  import: bool-def-imp
  import: bool-def-forall
  package: bool-def-or-1.0
}

bool-def-false {
  import: bool-def-forall
  package: bool-def-false-1.0
}

bool-def-not {
  import: bool-def-imp
  import: bool-def-false
  package: bool-def-not-1.0
}

bool-def-exists-unique {
  import: bool-def-and
  import: bool-def-imp
  import: bool-def-forall
  import: bool-def-exists
  package: bool-def-exists-unique-1.0
}

bool-def-cond {
  import: bool-def-true
  import: bool-def-imp
  import: bool-def-false
  package: bool-def-cond-1.0
}

bool-def-let {
  package: bool-def-let-1.0
}

main {
  import: bool-def-true
  import: bool-def-and
  import: bool-def-imp
  import: bool-def-forall
  import: bool-def-exists
  import: bool-def-or
  import: bool-def-false
  import: bool-def-not
  import: bool-def-exists-unique
  import: bool-def-cond
  import: bool-def-let
}
