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
(use-package use-package-ensure-system-package :ensure t)

;; Setup our custom file for local state
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load-file custom-file)

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
   "fs" 'save-buffer
   "w" evil-window-map
   )
  )

