;;; init-editor.el ---
;;                                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2021  David Vázquez Púa

;; Author: David Vázquez Púa <davazp@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(use-package smartparens
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  :config
  ;; Without this, smartparens keybinings will break
  ;; variables like `org-special-ctrl-k'.
  (add-to-list 'sp-ignore-modes-list 'org-mode)
  (smartparens-global-strict-mode)
  (sp-use-paredit-bindings))

(use-package expand-region)
(use-package multiple-cursors)

;; Smarter cursor movement, by moving first to the indentation point
;; and then to the beginning of line, for example.
(use-package mwim
  :general
  ("C-a" 'mwim-beginning
   "C-e" 'mwim-end))

(use-package whitespace
  :straight (:type built-in)
  :diminish global-whitespace-mode
  :config
  (global-whitespace-mode)
  (setq whitespace-global-modes '(not dired-mode magit-status-mode magit-process-mode))
  (setq whitespace-style
      '(face
        tabs
        trailing
        space-before-tab
        empty
        space-after-tab
        tab-mark)))

;; Similar to the command `whitespace-cleanup'. However, this cleans
;; up the buffer only if it was clean to start with. Preventing you
;; from creating messy diffs.
(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-nav-mode))


(provide 'init-editor)
;;; init-editor.el ends here
