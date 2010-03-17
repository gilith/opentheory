require thm {
  package: hol-light-thm-2009.8.24
}

require example-pos {
  import: thm
  package: example-pos-2009.8.24
}

theory {
  import example-pos;
}
