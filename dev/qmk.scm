(load "../utilities.scm")
(use-modules (utilities)
             (gnu packages embedded))
(specifications->package-list
 "qmk" (make-arm-none-eabi-nano-toolchain-12.3.rel1))
