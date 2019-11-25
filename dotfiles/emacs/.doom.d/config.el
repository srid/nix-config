;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-theme 'doom-moonlight)
(setq doom-font (font-spec :family "Fantasque Sans Mono" :size 30))

;; Markdown - line wrapping sensible defaults
(remove-hook 'text-mode-hook #'auto-fill-mode)
(remove-hook 'markdown-mode-hook #'auto-fill-mode)
(add-hook 'markdown-mode-hook #'visual-line-mode)
;; Markdown - flycheck using mmark
;; (require 'flycheck-mmark)
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-mmark-setup))
;; (add-hook 'markdown-mode-hook #'flycheck-mode)

(add-hook 'haskell-mode #'ormolu-mode)
(setq-hook! 'haskell-mode ormolu-reformat-buffer-on-save t)
(after! haskell-mode
  ;; rhyolite quasi quotes
  (setq haskell-font-lock-quasi-quote-modes
        (append haskell-font-lock-quasi-quote-modes
                '(("queryQ" . sql-mode)
                  ("executeQ" . sql-mode)
                  ("traceQueryQ" . sql-mode)
                  ("traceExecuteQ" . sql-mode)
                  ("sqlQ" . sql-mode))))

  ;; (setq haskell-process-type 'cabal-new-repl)
  ;; (setq lsp-haskell-process-wrapper-function
  ;;       (lambda (args)
  ;;         (append
  ;;         (append (list "nix-shell" "-I" "." "--command" )
  ;;                 (list (mapconcat 'identity args " "))
  ;;                 )
  ;;         (list (nix-current-sandbox))
  ;;         )
  ;;         ))
  )

(map!
  (:map vterm-mode-map
    ;; Easier window movement
    :i "C-h" #'evil-window-left
    :i "C-j" #'evil-window-down
    :i "C-k" #'evil-window-up
    :i "C-l" #'evil-window-right))
