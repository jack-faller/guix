#!/usr/bin/env guile
!#

(use-modules (scheme base))

(define greek
  '(("a" "α")
    ("b" "β")
    ("g" "γ") ("G" "Γ")
    ("d" "δ") ("D" "Δ")
    ("e" "ε") ("E" "ϵ")
    ("z" "ζ")
    ("n" "η")
    ("o" "θ") ("O" "Θ")
    ("i" "ι")
    ("k" "κ")
    ("l" "λ") ("L" "Λ")
    ("m" "μ")
    ("v" "ν")
    ("x" "ξ") ("X" "Ξ")
    ("p" "π") ("P" "Π")
    ("r" "ρ")
    ("s" "σ") ("S" "Σ")
    ("t" "τ")
    ("u" "υ")
    ("f" "φ") ("|o" "ɸ") ("F" "Φ")
    ("c" "χ")
    ("{" "ψ") ("}" "Ψ")
    ("w" "ω") ("W" "Ω")))

(define blackboard
  '(("C" "ℂ")
    ("H" "ℍ")
    ("N" "ℕ")
    ("P" "ℙ")
    ("Q" "ℚ")
    ("R" "ℝ")
    ("Z" "ℤ")))

(define math-rules
  '(("prod" "∏")
    ("sum" "∑")
    ("all" "∀")
    ("cup" "∪")
    ("cap" "∩")
    ("cop" "⨿")
    ("land" "∧")
    ("lor" "∨")
    ("u[" "⌈")
    ("u]" "⌉")
    ("d[" "⌊")
    ("d]" "⌋")
    ("[|" "⟦")
    ("|]" "⟧")
    ("top" "⊤")
    ("tac" "⊥")
    ("from" "↤") ("maps" "↦")
    ;; collides with †
    ;; ("<-|" "↤") ("|->" "↦")
    ;; ("<--|" "⟻") ("|-->" "⟼")
    ))

(define math-negatable
  '(("ex" "∃" "∄")
    ("div" "∣" "∤")
    ;; Collides with †
    ;; ("|-" "⊢" "⊬")
    ("turn" "⊢" "⊬")
    ("-|" "⊣" #f)
    ("|=" "⊨" "⊭")
    ("||-" "⊩" "⊮")
    ("||=" "⊫" "⊯")
    ("|||-" "⊪" #f)
    ("in" "∈" "∉") ("ni" "∋" "∌")
    ("~" #f "≁")
    ("<|" "⊲" "⋪") ("|>" "⊳" "⋫")
    ("_<|" "⊴" "⋬") ("_|>" "⊵" "⋭")
    ("~_" "≃" "≄") ("_~" "≃" "≄")
    ("~=" "≅" "≆")
    ("~~" "≈" "≉")
    ("eq" "=" "≠") ("=" "=" "≠")
    ("_=" "≡" "≢")
    ("le" "≤" "≰") ("<=" "≤" "≰")
    ;; clashes with greek e
    ;; ("ge" "≥" "≱")
    (">=" "≥" "≱")
    ("lt" #f "≮") ("<" #f "≮")
    ("gt" #f "≯") (">" #f "≯")
    ("subs" "⊂" "⊄") ("sups" "⊃" "⊅")
    ("sube" "⊆" "⊈") ("supe" "⊇" "⊉")
    ("img" "⊏" #f) ("orig" "⊐" #f)
    ("ime" "⊑" "⋢") ("orie" "⊒" "⋣")))
;; NOT COVERED
;; # <Multi_key> <U2276> <slash> : "≸"
;; # <Multi_key> <U2277> <slash> : "≹"
;; # <Multi_key> <U227A> <slash> : "⊀"
;; # <Multi_key> <U227B> <slash> : "⊁"
;; # <Multi_key> <U227C> <slash> : "⋠"
;; # <Multi_key> <U227D> <slash> : "⋡"

(define big-math
  '(("land" "⋀")
    ("lor" "⋁")
    ("cup" "⋃")
    ("cap" "⋂")
    ("cop" "∐")
    ("<" "⟨")
    (">" "⟩")
    ("<<" "⟪")
    (">>" "⟫")))

;; missing £$%&#@ tab
(define table
  '(("~" . "asciitilde")
    (" " . "space")
    ("-" . "dash")
    ("+" . "plus")
    ("_" . "underscore")
    ("=" . "equal")
    ("/" . "slash")
    ("\\" . "backslash")
    ("|" . "bar")
    ("[" . "bracketleft")
    ("]" . "bracketright")
    ("(" . "parenleft")
    (")" . "parenright")
    ("{" . "braceleft")
    ("}" . "braceright")
    ("'" . "quot")
    ("\"" . "quotdouble")
    ("`" . "grave")
    ("<" . "less")
    (">" . "greater")
    ("*" . "asterisk")
    ("." . "period")
    ("." . "comma")
    ("^" . "asciicircum")
    ("%" . "percent")
    (":" . "colon")
    (";" . "semicolon")
    ("?" . "question")
    ("!" . "exclam")))

(define (apply-map f list)
  (map (lambda (args) (apply f args)) list))
(define (apply-each f list)
  (for-each (lambda (args) (apply f args)) list))

(define (prefix-rule start)
  (lambda (code result)
    (list (string-append start code) result)))

(define all-rules
  (append
   (apply-map (prefix-rule "g") greek)
   math-rules
   (apply-map (prefix-rule "mb") blackboard)
   (apply-map (prefix-rule "big") big-math)
   (apply
    append!
    (apply-map
     (lambda (code result slashed)
       (define a (if result (list (list code result)) '()))
       (if slashed
           (cons (list (string-append "no" code) slashed) a)
           a))
     math-negatable))))

(define sorted
  (sort all-rules (lambda (a b) (string<? (car a) (car b)))))

(for-each (lambda (a)
            (when (string-contains (car a) " ")
              (error "Rule has spaces:" (car a))))
          sorted)

(let loop ((rules sorted))
  (when (not (null? (cdr rules)))
    (when (string=? (caar rules) (caadr rules))
      (error "Duplicate rule:" (caar rules)))
    (when (string-prefix? (caar rules) (caadr rules))
      (set-car! (car rules) (string-append (caar rules) " ")))
    (loop (cdr rules))))

(define (rule code result)
  (format #t "<Multi_key>")
  (string-for-each
   (lambda (c)
     (define s (string c))
     (format #t " <~a>" (or (assoc-ref table s) s)))
   code)
  (display " : \"")
  (write-bytevector (string->utf8 result))
  (display "\"\n"))
;; TODO: read codes from default file and merge duplicates
(format #t "include \"%L\"\n")
(apply-each rule sorted)
