(load "../utilities.scm")
(use-modules (utilities))
(apply
 specifications->package-list
 (map
  (lambda (tex-package) (string-append "texlive-" tex-package))
  '("scheme-basic" "collection-latexrecommended" "collection-fontsrecommended"
    "ulem"
    "biblatex" "biber"
    "hyperref" "cleveref"
    "pgf" "pgfgantt"
    "enumitem"
    ;; Maths fonts.
    "amsfonts")))
