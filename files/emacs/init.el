;; -*- lexical-binding: t -*-

;; This won't set the variable in Guix as it will have already been defvar-ed t.
(defvar using-guix nil)

;; setup straight to load org mode 
(defvar bootstrap-version)
;; (defvar all-the-icons-fonts-installed? t)
(setf display-warning-minimum-level :error
      ;straight-vc-git-default-protocol 'ssh
      )
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    ;; (setf all-the-icons-fonts-installed? nil)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defmacro after-load (modes &rest body)
  "Like with-eval-after-load, but also except a list of modes."
  (declare (indent 1))
  (when (not (listp modes)) (setf modes (list modes)))
  (push 'progn body)
  (dolist (i modes)
    (setf body `(with-eval-after-load ',i ,body)))
  body)

(require 'cl-lib)
(cl-defmacro pkg (name &optional &key require github)
  (list
   'progn
   (if github
       `(straight-use-package '(,name
                                :type git
                                :host github
                                :repo ,github))
     `(straight-use-package ',name))
   (if require `(require ',name) nil)
   `',name))
