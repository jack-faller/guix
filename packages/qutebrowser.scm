(define-module (packages qutebrowser)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gnome)
  #:use-module (ice-9 match))

(define qt-package-base
  (package
    (inherit qtsvg)
    (arguments
     (substitute-keyword-arguments (package-arguments qtsvg)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'delete-installed-tests
              (lambda _
                (define file
                  (string-append #$output "/tests"))
                (when (file-exists? file)
                  (delete-file-recursively file))))))))))

(define (qt-url component version)
  "Return a mirror URL for the Qt COMPONENT at VERSION."
  ;; We can't use a mirror:// scheme because these URLs are not exact copies:
  ;; the layout differs between them.
  (let ((x (match (version-major version)
             ("5" "-everywhere-opensource-src-")
             ;; Version 6 and later dropped 'opensource' from the archive
             ;; names.
             (_ "-everywhere-src-"))))
    (string-append "mirror://qt/qt/"
                   (version-major+minor version)
                   "/"
                   version
                   "/submodules/"
                   component
                   x
                   version
                   ".tar.xz")))

(define-public qtconnectivity-6
  (package
    (inherit qt-package-base)
    (name "qtconnectivity")
    (version "6.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (qt-url name version))
       (sha256
        (base32 "16fwbz9pr6pi19119mp6w0crq9nsb35fw8cgpfpkq99d6li4jbnv"))))
    (native-inputs (list perl pkg-config qtdeclarative))
    (inputs (list bluez qtbase))
    (synopsis "Qt Connectivity module")
    (description "The Qt Connectivity modules provides modules for interacting
with Bluetooth and NFC.")))

(define-public qtsensors-6
  (package
    (inherit qt-package-base)
    (name "qtsensors")
    (version "6.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (qt-url name version))
       (sha256
        (base32 "19iamfl4znqbfflnnpis6qk3cqri7kzbg0nsgf42lc5lzdybs1j0"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qt-package-base)
       ((#:parallel-tests? _ #f)
        ;; can lead to race condition
        #f)))
    (native-inputs (list perl qtdeclarative))
    (inputs (list qtbase))
    (synopsis "Qt Sensors module")
    (description
     "The Qt Sensors API provides access to sensor hardware via QML
and C++ interfaces.  The Qt Sensors API also provides a motion gesture
recognition API for devices.")))

(define-public qtserialport-6
  (package
    (inherit qt-package-base)
    (name "qtserialport")
    (version "6.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (qt-url name version))
       (sha256
        (base32 "17nc5kmha6fy3vzkxfr2gxyzdsahs1x66d5lhcqk0szak8b58g06"))))
    (native-inputs (list perl pkg-config))
    (inputs (list qtbase eudev libxkbcommon vulkan-headers))
    (arguments
     (substitute-keyword-arguments (package-arguments qt-package-base)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'patch-dlopen-paths
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "src/serialport/qtudev_p.h"
                  ;; Use the absolute paths for dynamically loaded libs,
                  ;; otherwise the lib will be searched in LD_LIBRARY_PATH which
                  ;; typically is not set in guix.
                  (("setFileNameAndVersion\\(QStringLiteral\\(\"udev\")")
                   (format #f "setFileNameAndVersion(QStringLiteral(~s))"
                           (string-append #$(this-package-input "eudev")
                                          "/lib/libudev"))))))))))
    (synopsis "Qt Serial Port module")
    (description "The Qt Serial Port module provides the library for
interacting with serial ports from within Qt.")))

;; Won't build for some reason (maybe related to qtpositioningquick?)
(define-public qtlocation-6
  (package
    (inherit qt-package-base)
    (name "qtlocation")
    (version "6.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (qt-url name version))
       (sha256
        (base32 "1yvdv1gqj7dij7v4cq9rlnqfb77c0v9b7n56jccvy5v6q9j7s7c9"))))
    (arguments
     (substitute-keyword-arguments (package-arguments qt-package-base)
       ((#:tests? _ #f)
        #f) ;TODO: Enable the tests
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'check 'pre-check
              (lambda _
                (setenv "HOME" "/tmp")))))))
    (native-inputs (list perl))
    (inputs (list icu4c
                  openssl
                  qtbase
                  zlib
                  qtdeclarative
                  qtpositioning
                  libxkbcommon
                  vulkan-headers))
    (synopsis "Qt Location and Positioning modules")
    (description "The Qt Location module provides an interface for location,
positioning and geolocation plugins.")))

(define-public python-pyqtwebengine-6
  (package
    (name "python-pyqtwebengine")
    (version "6.5.0")
    (source
     (origin
       (method url-fetch)
       ;; The newest releases are only available on PyPI.  Older ones
       ;; are mirrored at the upstream home page.
       (uri (list (pypi-uri "PyQt6_WebEngine" version)
                  ;; This URL is wrong but I can't find the right one.
                  (string-append "https://www.riverbankcomputing.com/static"
                                 "/Downloads/PyQtWebEngine/"
                                 version
                                 "/PyQtWebEngine-"
                                 version
                                 ".tar.gz")))
       (sha256
        (base32 "1sbfdl5p2g64vlkm021z25lxqqsvbsikqrxizaia50f1qibdpacb"))))
    (build-system pyproject-build-system)
    (native-inputs (list python python-sip python-pyqt-builder
                         ;; qtbase is required for qmake
                         qtbase))
    (inputs `(("python" ,python-wrapper)
              ("python-sip" ,python-sip)
              ("python-pyqt" ,python-pyqt-6)
              ("qtbase" ,qtbase)
              ("qtsvg" ,qtsvg)
              ("qtdeclarative" ,qtdeclarative)
              ("qtwebchannel" ,qtwebchannel)
              ("qtwebengine" ,qtwebengine)))
    (arguments
     (list
      #:tests? #f ;No tests.
      #:configure-flags #~`(@ ("--verbose" . "") ;Print commands run.
                              ("--jobs" unquote
                               (number->string (parallel-job-count))))
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'set-include-dirs
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let* ((python (assoc-ref inputs "python"))
                              (python-pyqt (assoc-ref inputs "python-pyqt"))
                              (sip-include-dirs (string-append python-pyqt
                                                 "/lib/python"
                                                 (python-version python)
                                                 "/site-packages/PyQt6/bindings")))
                         (setenv "SIP_INCLUDE_DIRS" sip-include-dirs)))))))
    (home-page
     "https://www.riverbankcomputing.com/software/pyqtwebengine/intro")
    (synopsis "Python bindings for QtWebEngine")
    (description
     "PyQtWebEngine is a set of Python bindings for The Qt Company's Qt
WebEngine libraries.  The bindings sit on top of PyQt6 and are implemented as a
set of three modules.  Prior to v5.12 these bindings were part of PyQt
itself.")
    (license license:gpl3)))

(define-public python-pyqt6-sip
  (package
    (name "python-pyqt-sip")
    (version "13.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyQt6_sip" version))
       (sha256
        (base32 "0y2pgc1kzskq3q230b5d48izvzy9dl4hkfjpcr7kv53ih1cf31i4"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;No test code.
    (home-page "https://www.riverbankcomputing.com/software/sip/")
    (synopsis "Sip module support for PyQt6")
    (description "Sip module support for PyQt6")
    (license license:lgpl2.1+)))

(define-public python-pyqt-6
  (package
    (name "python-pyqt")
    (version "6.5.2")
    (source
     (origin
       (method url-fetch)
       ;; PyPI is the canonical distribution point of PyQt.  Older
       ;; releases are available from the web site.
       (uri (list (pypi-uri "PyQt6" version)
                  (string-append "https://www.riverbankcomputing.com/static/"
                                 "Downloads/PyQt6/"
                                 version
                                 "/PyQt6-"
                                 version
                                 ".tar.gz")))
       (file-name (string-append "PyQt6-" version ".tar.gz"))
       (sha256
        (base32 "100jh1iiz5gx821qzgicfrqv7hjjj98pchdbc1nvdzzra1ryx1ql"))
       ;; TODO: Figure out if this is okay to remove
       ;; (patches (search-patches "pyqt-configure.patch"))
       ))
    (build-system pyproject-build-system)
    (native-inputs (list qtbase ;for qmake
                         python-pyqt-builder))
    (propagated-inputs (list python-sip python-pyqt6-sip))
    (inputs `(("python" ,python-wrapper)
              ("qtbase" ,qtbase)
              ("qtconnectivity" ,qtconnectivity-6)
              ("qtdeclarative" ,qtdeclarative)
              ;; I hope this wasn't necessary
              ;; ("qtlocation" ,qtlocation-6)
              ("qtmultimedia" ,qtmultimedia)
              ("qtsensors" ,qtsensors-6)
              ("qtserialport" ,qtserialport-6)
              ("qtsvg" ,qtsvg)
              ("qttools" ,qttools)
              ("qtwebchannel" ,qtwebchannel)
              ("qtwebsockets" ,qtwebsockets)))
    (arguments
     (list
      #:tests? #f ;No tests.
      #:configure-flags #~`(@ ("--verbose" . "") ;Print commands run.
                              ("--confirm-license" . "")
                              ("--jobs" unquote
                               (number->string (parallel-job-count))))
      #:phases #~(modify-phases %standard-phases
                   ;; When building python-pyqtwebengine, <qprinter.h> can not be
                   ;; included.  Here we substitute the full path to the header in the
                   ;; store.
                   (add-after 'unpack 'substitute-source
                     (lambda* (#:key inputs #:allow-other-keys)
                       (let* ((qtbase (assoc-ref inputs "qtbase"))
                              (qtprinter.h (string-append "\"" qtbase
                                            "/include/qt6/QtPrintSupport/qprinter.h\"")))
                         (substitute* (list "sip/QtPrintSupport/qprinter.sip"
                                       "sip/QtPrintSupport/qpyprintsupport_qlist.sip")
                           (("<qprinter.h>")
                            qtprinter.h))))))))
    (home-page "https://www.riverbankcomputing.com/software/pyqt/intro")
    (synopsis "Python bindings for Qt")
    (description
     "PyQt is a set of Python v2 and v3 bindings for the Qt application
framework.  The bindings are implemented as a set of Python modules and
contain over 620 classes.")
    (license license:gpl3)))

(define-public qutebrowser
  (package
    (name "qutebrowser")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/qutebrowser/"
                           "qutebrowser/releases/download/v"
                           version
                           "/"
                           "qutebrowser-"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "0prf9c7nx4aizfczjb0fpsn3alz210i6wc7s2jwb1mh8r8fcq3ah"))))
    (build-system python-build-system)
    (native-inputs (list python-attrs)) ;for tests
    (inputs (list bash-minimal
                  python-colorama
                  python-jinja2
                  python-markupsafe
                  python-pygments
                  python-pynacl
                  python-pypeg2
                  python-pyyaml
                  ;; FIXME: python-pyqtwebengine needs to come before python-pyqt so
                  ;; that it's __init__.py is used first.
                  python-pyqtwebengine-6
                  python-pyqt-6
                  ;; While qtwebengine is provided by python-pyqtwebengine, it's
                  ;; included here so we can wrap QTWEBENGINEPROCESS_PATH.
                  qtwebengine))
    (arguments
     `( ;FIXME: With the existance of qtwebengine, tests can now run.  But
       

       ;; they are still disabled because test phase hangs.  It's not readily
       ;; apparent as to why.
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-systemdir
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "qutebrowser/utils/standarddir.py"
                          (("/usr/share")
                           (string-append out "/share"))))))
                  (add-after 'unpack 'find-userscripts
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "qutebrowser/commands/userscripts.py"
                        (("os.path.join.*system=True)")
                         (string-append "os.path.join(\""
                                        (assoc-ref outputs "out")
                                        "\", \"share\", \"qutebrowser\"")))))
                  (add-before 'check 'set-env-offscreen
                    (lambda _
                      (setenv "QT_QPA_PLATFORM" "offscreen")))
                  (add-after 'install 'install-more
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (rename-file "misc/Makefile" "Makefile")
                        (substitute* "Makefile"
                          ((".*setup\\.py.*")
                           ""))
                        (invoke "make" "install"
                                (string-append "PREFIX=" out))
                        (delete-file-recursively (string-append out
                                                  "/share/metainfo")))))
                  (add-after 'install-more 'wrap-scripts
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (python (assoc-ref inputs "python"))
                             (path (string-append out "/lib/python"
                                                  ,(version-major+minor (package-version
                                                                         python))
                                                  "/site-packages:"
                                                  (getenv "GUIX_PYTHONPATH"))))
                        (for-each (lambda (file)
                                    (wrap-program file
                                      `("GUIX_PYTHONPATH" ":" prefix
                                        (,path))))
                                  (append (find-files (string-append out
                                                       "/share/qutebrowser/scripts")
                                                      "\\.py$")
                                          (find-files (string-append out
                                                       "/share/qutebrowser/userscripts")))))))
                  (add-after 'wrap 'wrap-qt-process-path
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (wrap-program (search-input-file outputs
                                                       "bin/qutebrowser")
                        `("QTWEBENGINEPROCESS_PATH" =
                          (,(search-input-file inputs
                             "/lib/qt6/libexec/QtWebEngineProcess")))
                        `("QTWEBENGINE_RESOURCES_PATH" =
                          (,(search-input-directory inputs
                                                    "/share/qt6/resources")))))))))
    (home-page "https://qutebrowser.org/")
    (synopsis "Minimal, keyboard-focused, vim-like web browser")
    (description "qutebrowser is a keyboard-focused browser with a minimal
GUI.  It is based on PyQt6 and QtWebEngine.")
    (license license:gpl3+)))
qutebrowser
