;; NOTE: All instances of `use-package` in this file will be processed by Nix (see ./emacs.nix) to automatically install the referenced packages when installing Emacs itself via home-manager. Thus, to "add" a package, we only need to insert the corresponding `use-package` expression in this file.

;; Sane defaults
(setq
 inhibit-startup-message t
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-hl-line-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Configure use-package to use system (nix) packages
(require 'package)
(setq package-archives nil)
(package-initialize)
(require 'use-package)

;; Setup our custom file for local state
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load-file custom-file)

;; Doom Themes
(use-package all-the-icons)
(use-package doom-themes
    :after all-the-icons
    :config
    (setq
    doom-themes-enable-bold t
    doom-themes-enable-italic t)
    (load-theme 'doom-vibrant t)
    (doom-themes-visual-bell-config))
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Essential UX 
(use-package selectrum
  :config
  (selectrum-mode +1)
  )
(use-package prescient
  :config
  (prescient-persist-mode +1)
  )
(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  )

;; VI
(use-package evil
  :config
  (evil-mode))
(use-package which-key
  :defer 0.1
  :init
  :config
  (which-key-mode +1)
  )
(use-package evil-leader
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode)
  (evil-leader/set-key
   "bd" 'kill-buffer
   "bb" 'consult-buffer
   "`" 'evil-switch-to-windows-last-buffer
   "fs" 'save-buffer
   "pf" 'project-find-file
   "pp" 'project-switch-project
   "gg" 'magit-status
   "w" evil-window-map
   )
  )

;; Project management
(use-package project)

;; Programming language modes & tools
(use-package nix-mode)
(use-package haskell-mode)
(use-package magit)

(load "/home/srid/nix-config/dep/markdown-mode/markdown-mode.el")
(use-package markdown-mode
  :config
  (setq markdown-enable-wiki-links t)
  (setq markdown-wiki-link-search-subdirectories t)
  (setq markdown-wiki-link-search-parent-directories t)
  (setq markdown-link-space-sub-char " ")
  ;; TODO: Use `neuron -d $project-root query --cached` to find IDs
  (setq sample-links
	'("touch" "touch_start" "for" "foreach"))
  (defun neuron-wiki-link-completion-at-point ()
    (interactive)
    (let* (
         (bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end sample-links . nil )))
  (define-derived-mode neuron-mode markdown-mode "Neuron"
    "Major mode for editing neuron Markdown notes"
    (add-hook
     'completion-at-point-functions
     'neuron-wiki-link-completion-at-point nil 'local)
    (defvar neuron-mode-map nil "Keymap for `neuron-mode'.")
    (progn
      (setq neuron-mode-map (make-sparse-keymap))
      (define-key neuron-mode-map (kbd "TAB") #'complete-symbol))
    (use-local-map neuron-mode-map)
    )
  )
