(load "../utilities.scm")
(use-modules (utilities))
(specifications->package-list
 "rust" "rust:tools" "rust:cargo" "rust-analyzer" "gdb" "cgdb")
