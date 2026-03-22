(load "../utilities.scm")
(load "../packages/rustup.scm")
(use-modules (utilities)
             (packages rustup)
             (guix packages)
             (guix gexp)
             (gnu packages commencement))

(cons*
 (package
   (inherit gcc-toolchain)
   (source (file-append gcc-toolchain "/bin"))
   (name "cc")
   (outputs '("out"))
   (build-system (@ (guix build-system copy) copy-build-system))
   (arguments
    (list #:install-plan #~'(("gcc" "bin/cc")))))
 (specifications->package-list rustup "rust-analyzer" "gdb" "cgdb"))

