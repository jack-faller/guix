(define-module (packages icons)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages fonts)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public font-google-material-design-icons-desktop
  (package
   (inherit font-google-material-design-icons)
   (name "font-google-material-design-icons-desktop")
   (source
	(origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/Templarian/MaterialDesign-Font")
           (commit "d72e308")))
     (sha256
      (base32
       "16v98s7x9z6fdz61mrawafcx3pm2m0ipjs9zbvq7sxi5p3zrqynv"))))))

(define-public font-crimson
  (package
   (name "font-crimson")
   (version "0.0.0")
   (source
	(origin
	 (method git-fetch)
	 (uri (git-reference
		   (url "https://github.com/skosch/Crimson")
		   (commit "685b5a2")))
	 (sha256
      (base32
       "0zs738isdn23p3g6gcd71bpdavsafizhchvkj20j3jh61zz0anar"))))
   (build-system font-build-system)
   ;; Open Font License 1.1
   (license #f)
   (synopsis "Crimson is a free and open-source text type family.")
   (description "The font is designed in the tradition of beautiful oldstyle type, and inspired particularly by the fantastic work of people like Jan Tschichold (Sabon), Robert Slimbach (Arno, Minion) and Jonathan Hoefler (Hoefler Text).")
   (home-page "https://github.com/skosch/Crimson")))

(define-public font-crimson-pro
  (package
   (inherit font-crimson)
   (name "font-crimson-pro")
   (source
	(origin
	 (method git-fetch)
	 (uri (git-reference
		   (url "https://github.com/Fonthausen/CrimsonPro")
		   (commit "24e8f7b")))
	 (sha256
	  (base32
	   "1jz60zf4fi2vvicyrkfgvhvwpjhk9qfk6djh266vf2qw0ga42cfz"))))
   (synopsis "Crimson Pro a professionally produced redesign of Crimson.")
   (description "Crimson Pro a professionally produced redesign of Crimson by Jacques Le Bailly (@Fonthausen), commissioned by Google. This new Crimson is a fresh take on the original version and the result of months of painstaking work to perfect colour, glyph balance and legibility.")
   (home-page "https://github.com/Fonthausen/CrimsonPro")))

(define-public font-cochineal
  (package
   (inherit font-crimson)
   (name "font-cochineal")
   (version "1.085")
   (source
	(origin
	 (method url-fetch)
	 (uri "https://mirrors.ctan.org/fonts/cochineal.zip")
	 (sha256
	  (base32
	   "1fpqamrn233j2bgrdvvzm17scqyds11icyc2s6664mxcwza167jg"))))
   (synopsis "Cochineal is a fork from the Crimson fonts (Roman, Italic, Bold, BoldItalic only) released under the OFL by Sebastian Kosch.")
   (description "These remarkable fonts are inspired by the famous oldstyle fonts in the garalde family (Garamond, Bembo) but, in the end, look more similar to Minion, though with smaller xheight and less plain in detail. The Crimson fonts on which these were based had roughly 4200 glyphs in the four styles mentioned above. Cochineal adds more than 1500 glyphs in those styles so that it is possible to make a TEX support collection that contains essentially all glyphs in all styles. Bringing the Semibold styles up the same level would have required adding about 2000 additional glyphs, which I could not even contemplate.")
   (home-page "https://www.ctan.org/tex-archive/fonts/cochineal/")))

(define-public quintom-cursor-theme
  (package
   (name "quintom-cursor-theme")
   (version "0.0.0")
   (source
	(origin
	 (method git-fetch)
	 (uri (git-reference
		   (url "https://gitlab.com/Burning_Cube/quintom-cursor-theme")
		   (commit "d23e57333e816033cf20481bdb47bb1245ed5d4d")))
	 (sha256
	  (base32
	   "1dg52hirccr3cd8nf785gcl96fqcpvakycrnqdxajqfn546kdrs9"))))
   (build-system copy-build-system)
   (arguments
	'(#:install-plan
	  '(("Quintom_Snow Cursors/Quintom_Snow" "share/icons/")
		("Quintom_Ink Cursors/Quintom_Ink" "share/icons/"))))
   (home-page "https://gitlab.com/Burning_Cube/quintom-cursor-theme")
   (synopsis "This is an x-cursor theme designed to look decent.")
   (description synopsis)
   (license license:gpl3)))

(list font-google-material-design-icons-desktop font-crimson font-crimson-pro quintom-cursor-theme font-cochineal)
