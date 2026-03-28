(load "../utilities.scm")
(use-modules (utilities)
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
 (specifications->package-list
  "rust" "rust:tools" "rust:cargo" "rust-analyzer" "gdb" "cgdb"))

