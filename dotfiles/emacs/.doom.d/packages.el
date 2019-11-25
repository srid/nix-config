;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! writeroom-mode)
(package! chocolate-theme)
(package! git-auto-commit-mode)
(package! flycheck-mmark)
(package! ormolu
  :recipe (:host github :repo "vyorkin/ormolu.el"))
(package! nix-sandbox)

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
