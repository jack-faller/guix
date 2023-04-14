(use-modules (gnu packages gcc)
			 (gnu packages gdb)
			 (gnu packages base)
			 (gnu packages llvm)
			 (gnu packages debug)
			 (gnu packages valgrind)
			 (gnu packages autotools)
			 (gnu packages build-tools)
			 (gnu packages commencement))
(list gcc-toolchain (list glibc "debug") binutils
	  clang automake autoconf bear valgrind gnu-make gdb cgdb); clang is for clangd
