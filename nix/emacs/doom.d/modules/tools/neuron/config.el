;;; tools/neuron/config.el -*- lexical-binding: t; -*-

(defface neuron-stub-face
  '((((class color) (min-colors 88) (background dark)) :foreground "#C16069" :underline "#C16069")
    (((class color) (min-colors 88) (background light)) :foreground "#C16069" :underline "#C16069")
    (((class color) :foreground "Red" :underline "Red"))
    (t :inherit neuron-title-overlay-face))
  "Face for stub links."
  :group 'neuron-faces)

(customize-set-variable 'neuron-default-zettelkasten-directory (expand-file-name "~/zettelkasten"))

(customize-set-variable 'neuron-default-tags (list "stub"))

(customize-set-variable 'neuron-tag-specific-title-faces '(("stub" neuron-stub-face)))

(defun search-zettelkasten ()
  "Search zettels by content."
  (interactive)
  (progn
    (+ivy-file-search :in (neuron-zettelkasten) :recursive nil :prompt "Search Zettelkasten: ")
    (neuron-mode)))

(defun find-file-in-zettelkasten ()
  "Find a file in the currently active zettelkasten."
  (interactive)
  (let ((default-directory (neuron-zettelkasten)))
    (counsel-find-file)))

(use-package! neuron-mode
  ; Enable link autocompletion globally
  :hook (neuron-mode . company-neuron-setup)
  :config
  (map! :leader
        (:prefix ("z" . "zettel")
          "z" #'neuron-new-zettel
          "e" #'neuron-edit-zettel
          "w" #'neuron-rib-watch
          "g" #'neuron-rib-generate
          "o" #'neuron-open-zettel
          "O" #'neuron-open-index
          "j" #'neuron-open-daily-notes
          "t" #'neuron-query-tags
          "r" #'neuron-refresh
          "c" #'neuron-edit-zettelkasten-configuration
                 
          ;;; Alternatively, bind all rib commands in a separate prefix
          ;; (:prefix ("r" . "rib")
          ;;   "w" #'neuron-rib-watch
          ;;   "g" #'neuron-rib-generate
          ;;   "s" #'neuron-rib-serve
          ;;   "o" #'neuron-rib-open-zettel
          ;;   "z" #'neuron-rib-open-z-index
          ;;   "k" #'neuron-rib-kill
          ;;   )
          )
        )

  (map! :map neuron-mode-map
        :localleader
        ;; Override markdown-mode's default behavior to handle neuron links
        :ni "o" #'neuron-follow-thing-at-point

        ;; You can also remove the "z" prefix but
        ;; be careful not to override default
        ;; markdown-mode bindings.
        (:prefix ("z" . "zettel")
          :ni "u" #'neuron-edit-uplink
          :ni "t" #'neuron-add-tag
          :ni "T" #'neuron-add-tags
          :ni "o" #'neuron-open-current-zettel
          :ni "l" #'neuron-create-and-insert-zettel-link
          :v  "L" #'neuron-create-zettel-from-selection
          :ni "s" #'neuron-insert-static-link
          :ni "c" #'neuron-toggle-connection-type
          )
        )
              
       (map! :leader "sz" #'search-zettelkasten)
       (map! :leader "fz" #'find-file-in-zettelkasten)
  )