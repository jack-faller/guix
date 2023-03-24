(use-modules (gnu packages gcc)
			 (gnu packages commencement)
			 (gnu packages base)
			 (gnu packages llvm)
			 (gnu packages debug)
			 (gnu packages valgrind)
			 (gnu packages autotools)
			 (gnu packages build-tools))
(list gcc-toolchain (list glibc "debug") binutils
	  clang automake autoconf bear valgrind gnu-make cgdb); clang is for clangd
