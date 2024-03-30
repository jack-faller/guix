(load "../utilities.scm")
(use-modules (utilities))
(specifications->package-list
 "gcc-toolchain" "glibc:debug" "binutils" "clang" "automake" "autoconf" "bear"
 "valgrind" "make" "gdb" "cgdb")
