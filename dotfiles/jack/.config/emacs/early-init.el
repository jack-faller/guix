;; -*- lexical-binding: t; -*-
(setq package-enable-at-startup nil)

(when (eq system-type 'android)
  (setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		         (getenv "PATH")))
  (push "/data/data/com.termux/files/usr/bin" exec-path))

;; disable gc during startup and set to a higher number after
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 100000000)))
