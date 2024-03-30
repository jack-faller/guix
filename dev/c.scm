(load "../file-utils.scm")
(use-modules (file-utils))
(specifications->package-list
 "gcc-toolchain" "glibc:debug" "binutils" "clang" "automake" "autoconf" "bear"
 "valgrind" "make" "gdb" "cgdb")
