;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Luciano Laratelli <luciano@laratel.li>
;;; Copyright © 2024 Connor Clark <connor@psyleft.com>
;;; Copyright © 2024 Ian FitzPatrick <ian@ianfitzpatrick.eu>
;;;
;;; This file is NOT part of GNU Guix, but is supposed to be used with GNU
;;; Guix and thus has the same license.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (packages nvidia-container-toolkit)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages check)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages linux)
  #:use-module (nongnu packages nvidia)
  )

(define nvc-version "1.18.2")
(define libnvidia-container-origin
  (origin
    (method git-fetch)
    (uri (git-reference
           (url "https://github.com/NVIDIA/libnvidia-container")
           (commit (string-append "v" nvc-version))))
    (file-name (git-file-name "libnvidia-container" nvc-version))
    (patches (list (local-file "libnvidia-container.patch")))
    (sha256
     (base32
      "1jbpi49z27009c64cxqf9y6xy6qbs7j8lg1nv1zdsfhvjm58gccn"))))

(define-public nvidia-modprobe
  (package
    (name "nvidia-modprobe")
    (version "550.54.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NVIDIA/nvidia-modprobe")
                    (commit version)))
              (patches
               (list
                (file-append
                 libnvidia-container-origin "/mk/nvidia-modprobe.patch")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1a7q03pnwk3wa0p57whwv2mvz60bv77vvvaljqzwnscpyf94q548"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
	      (delete 'configure)
          (add-before 'build 'set-correct-cflags
	        (lambda* (#:key inputs outputs #:allow-other-keys)
    	      (setenv "CFLAGS" "-fPIC")
	          (display "setting CFLAGS\n")))
	      (add-after 'build 'build-static-link-libraries
	        (lambda* (#:key inputs outputs #:allow-other-keys)
	          (invoke "ar" "rcs" "_out/Linux_x86_64/libnvidia-modprobe-utils.a" "_out/Linux_x86_64/nvidia-modprobe-utils.o" "_out/Linux_x86_64/pci-sysfs.o")
	          (copy-recursively "_out/Linux_x86_64/" (string-append #$output "/lib"))))
	      (delete 'check)
	      (add-after 'patch-source-shebangs 'replace-prefix
	        (lambda* (#:key inputs outputs #:allow-other-keys)
    	      (setenv "CC" "gcc")
	          (setenv "PREFIX" #$output)
	          (copy-recursively "modprobe-utils/" (string-append #$output "/include")))))
      #:tests? #f))
    (native-inputs
     (list gcc-toolchain m4))
    (synopsis "Load the NVIDIA kernel module and create NVIDIA character device files")
    (description "Load the NVIDIA kernel module and create NVIDIA character device files")
    (home-page "https://github.com/NVIDIA/nvidia-modprobe")
    (license gpl2)))

(define-public libnvidia-container
  (package
    (name "libnvidia-container")
    (version nvc-version)
    (source libnvidia-container-origin)
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags `'("WITH_LIBELF=yes" "prefix="
                      ,(string-append "GIT_TAG=" version)
                      ,(string-append "REVISION=" version))
      #:phases
      #~(modify-phases %standard-phases
	      (delete 'configure)
	      (delete 'build)
	      (delete 'check)
          (add-after 'unpack 'ensure-writable-source
	        (lambda* (#:key inputs outputs #:allow-other-keys)
      	      (setenv "HOME" "/tmp")
	          (make-file-writable "src/ldcache.c")
      	      (make-file-writable "src/ldcache.h")
              (make-file-writable "src/nvc_info.c")))
	      (add-after 'patch-source-shebangs 'replace-prefix
	        (lambda* (#:key inputs outputs #:allow-other-keys)
	          (setenv "DESTDIR" (assoc-ref outputs "out"))
	          (substitute* "Makefile"
	            (("debug??libdir?") "debug") ;ensure debug files get installed in the correct subdir
	            ((".*nvidia-modprobe.mk.*") "\n")
	            (("^all: shared static tools") "all: shared tools")
	            ((".*LIB_STATIC.*libdir.*$") ""))
     	      (substitute* "mk/nvcgo.mk"
	            ((".*-rf.*")
	             "\tmkdir -p ${SRCS_DIR} && echo \"sources dir: ${SRCS_DIR}\"\n")
	            (("CURDIR./src/..PREFIX.")
	             "CURDIR)/src/$(PREFIX)/*")) ;deleting sources fails
              (substitute* "src/cli/libnvc.c"
	            (("libnvidia-ml.so.1")
	             "/run/current-system/profile/lib/libnvidia-ml.so.1"))
              (substitute* "src/nvc_internal.h"
	            (("libnvidia-ml.so.1")
	             "/run/current-system/profile/lib/libnvidia-ml.so.1"))
	          (setenv "C_INCLUDE_PATH" (string-append (getenv "C_INCLUDE_PATH") ":" #$libtirpc "/include/tirpc"))
     	      (setenv "LIBRARY_PATH" (string-append (getenv "LIBRARY_PATH") ":" #$libtirpc "/lib"))
              (setenv "LDFLAGS" (string-append (or (getenv "LDFLAGS") "") " -ltirpc -lseccomp -lcap -Wl,-rpath=" (assoc-ref outputs "out") "/lib"))
              (setenv "CFLAGS" (string-append (or (getenv "CFLAGS") "") " -DWITH_TIRPC -g"))
              (setenv "CC" "gcc"))))
      #:tests? #f))
    (native-inputs
     (list git-minimal curl which pkgconf pkg-config go-1.26))
    (inputs
     (list libseccomp nvidia-modprobe libtirpc libcap libelf docker rpcsvc-proto))
    (synopsis "Build and run containers leveraging NVIDIA GPUs")
    (description "The NVIDIA Container Toolkit allows users to build and run GPU accelerated containers. The toolkit includes a container runtime library and utilities to automatically configure containers to leverage NVIDIA GPUs.")
    (home-page "https://github.com/NVIDIA/nvidia-container-toolkit")
    (license asl2.0)))


(define-public nvidia-container-toolkit
  (package
    (name "nvidia-container-toolkit")
    (version nvc-version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/NVIDIA/nvidia-container-toolkit")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (patches (list (local-file "nvidia-container-toolkit.patch")))
              (sha256
               (base32
		"0mmqgj18zdjs9632v3fb340ywxj9fxac254n4gyzm35r1qhkphrq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.26
      #:import-path "github.com/NVIDIA/nvidia-container-toolkit"
      #:phases
      #~(modify-phases %standard-phases
  	      (add-after 'unpack 'fix-paths
	        (lambda* (#:key inputs outputs #:allow-other-keys)
	          (substitute* "src/github.com/NVIDIA/nvidia-container-toolkit/internal/config/config.go"
                (("/usr/bin")
		         (string-append (assoc-ref outputs "out") "/bin")))))
          (replace 'build
            (lambda arguments
              (for-each
               (lambda (directory)
		         (apply (assoc-ref %standard-phases 'build)
			            (append arguments (list #:import-path directory))))
               '("github.com/NVIDIA/nvidia-container-toolkit/cmd/nvidia-ctk"
                 "github.com/NVIDIA/nvidia-container-toolkit/cmd/nvidia-container-runtime"
                 "github.com/NVIDIA/nvidia-container-toolkit/cmd/nvidia-container-runtime-hook"))))

          ;; (add-after 'build 'set-library-path
          ;;   (lambda* (#:key inputs outputs #:allow-other-keys)
          ;;     (use-modules (ice-9 ftw))
          ;;     (define path (string-append (assoc-ref outputs "out") "/bin"))
          ;;     (for-each
          ;;      (lambda (file)
          ;;        (wrap-program (string-append path "/" file)
          ;;          '("NVIDIA_CTK_CDI_GENERATE_LIBRARY_SEARCH_PATHS"
          ;;            = ("/run/current-system/profile/lib/"))
          ;;          '("NVIDIA_CTK_CDI_GENERATE_LIBRARY_SEARCH_PATHS"
          ;;            = ("/run/current-system/profile/lib/"))
          ;;          ))
          ;;      (scandir path (lambda (name) (not (string-prefix? "." name)))))))
          )
      #:tests? #f
      #:install-source? #f))
    (propagated-inputs
     (list libnvidia-container))
    (synopsis "Build and run containers leveraging NVIDIA GPUs")
    (description "The NVIDIA Container Toolkit allows users to build and run GPU accelerated containers. The toolkit includes a container runtime library and utilities to automatically configure containers to leverage NVIDIA GPUs.")
    (home-page "https://github.com/NVIDIA/nvidia-container-toolkit")
    (license asl2.0)))

nvidia-container-toolkit
