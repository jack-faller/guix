(add-to-load-path (dirname (current-filename)))
(use-modules (guix gexp)
             (gnu home)
             (gnu home services))

(home-environment)
