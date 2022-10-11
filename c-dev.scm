(use-modules (gnu packages gcc)
			 (gnu packages base)
			 (gnu packages llvm)
			 (gnu packages autotools)
			 (gnu packages build-tools))
(list gcc glibc binutils clang automake autoconf bear) ; clang is for clangd
