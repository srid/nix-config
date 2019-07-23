;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-theme 'doom-one-light)
(setq doom-font (font-spec :family "Fira Code" :size 26))

(after! haskell-mode
  ;; rhyolite quasi quotes
  (setq haskell-font-lock-quasi-quote-modes
        (append haskell-font-lock-quasi-quote-modes
                '(("queryQ" . sql-mode)
                  ("executeQ" . sql-mode)
                  ("traceQueryQ" . sql-mode)
                  ("traceExecuteQ" . sql-mode)
                  ("sqlQ" . sql-mode)))))
