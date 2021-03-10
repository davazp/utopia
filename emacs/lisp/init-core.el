;;; init.el ---                                      -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t))

(setq user-full-name "David Vázquez Púa")
(setq user-mail-address "davazp@gmail.com")

(setq default-directory "~/")

;; This package will customize many packages for us to write their
;; files to ~/.emacs.d/var and ~/.emacs.d/etc, instead of cluttering
;; completely ~/.emacs and the ~.
(use-package no-littering
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package general)

;;
;; General
;;
(setq make-backup-files nil)

;; Mac adjustmenets
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-function-modifier 'hyper)

(use-package restart-emacs)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(use-package envrc)

;;
;; Performance
;;

;; Recommended by lsp-mode and with M-x lsp-doctor.
;;
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;;
;; UI
;;

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t
      inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "davazp")

;; Disable audible or visible bells
(setq visible-bell nil
      ring-bell-function 'ignore)

;; Change the title bar to be of the same color as the
;; background color set up by theme.
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; Allow to diminish minor modes so they do not clutter the modeline
(use-package diminish
  :diminish eldoc-mode)

(use-package emojify
  :hook (after-init . global-emojify-mode))

;; Highlight TODO, FIXME and NOTE in comments.
(use-package hl-todo
  :config
  (global-hl-todo-mode 1))

(use-package zoom-frm)

(use-package presentation)
(use-package focus)

(use-package dimmer
  :config
  (dimmer-configure-which-key)
  (dimmer-mode))

;; Display a clock in the modeline
(display-time-mode)



;;
;; Window management
;;

;; Use popup windows for some help buffers, so they don't stick around
;; and messup the window configuration.
(use-package popwin
  :config
  (popwin-mode))

;;
;; Keybindings
;;

(use-package keyfreq
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package org
  :config
  (setq org-odd-levels-only t)
  (setq org-special-ctrl-k t)
  (setq org-special-ctrl-a/e t))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))


;; Dim parenthesis
(use-package paren-face)
(global-paren-face-mode 1)


;;
;; Editor
;;


;; Ensure that M-a and M-e will stop after a dot, even without two
;; spaces after it.
(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)


(use-package saveplace
  :straight (:type built-in)
  :config
  (save-place-mode))

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


(use-package wgrep)


(use-package whitespace
  :straight (:type built-in)
  :diminish global-whitespace-mode
  :config
  (global-whitespace-mode)
  (setq whitespace-global-modes '(not dired-mode))
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

;;
;; Ivy/Counsel/Swiper completion
;;

(use-package recentf
  :config
  (setq recentf-max-saved-items 1000)
  )

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'abbreviate)
  (ivy-mode))

(use-package counsel
  :diminish counsel-mode
  :config
  (counsel-mode))

(define-key isearch-mode-map (kbd "M-i") 'swiper-from-isearch)

;; A better sorting and filtering of ivy candidates.
(use-package ivy-prescient
  :config
  (ivy-prescient-mode))

;; Provide extra information in the ivy's output. For example, adds
;; docstrings to elisps commands and variables.
(use-package ivy-rich
  :config
  (ivy-rich-mode)
  ;; I have no idea what this does, but it is recommended in the ivy-rich website
  ;; https://github.com/Yevgnen/ivy-rich
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))


;;
;; Language Server Protocol support
;;

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l"))

(use-package lsp-ui)
(use-package company
  :diminish company-mode
  :config
  (global-company-mode))

;;
;; Linting & Formatting
;;

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package format-all
  :diminish format-all-mode
  :commands (format-all-buffer format-all-mode)
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("Nix" nixfmt)
                  ("JavaScript" prettier)
                  ("TypeScript" prettier)
                  ("SQL" (pgformatter "-s2")))))


;;
;; Projects
;;

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-project-search-path '("~/Projects")))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))


;;
;; Tools
;;
(use-package magit)
(use-package forge
  :after magit)

(use-package vterm)

;; Dired
(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook '(dired-mode . all-the-icons-dired-mode))


;; Editor settings
(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package prodigy)

(use-package docker)


;;
;; Languages
;;
(use-package typescript-mode
  :mode "\\.tsx\\'"
  :hook (typescript-mode . subword-mode)
  :hook (typescript-mode . lsp)
  :config
  (setq typescript-indent-level 2))

(use-package add-node-modules-path
  :config
  ;;
  ;; We can't enable add-node-modules-path directly. Instead, we set
  ;; it up to add node_modules to the path before saving the file. We
  ;; have to make sure this is done before format-all reads it though.
  ;;
  ;; This is necessary to prevent envrc overriding the value. See
  ;;
  ;;    https://github.com/purcell/envrc/issues/19
  ;;
  (add-hook 'typescript-mode-hook 'davazp/setup-node-modules-path)
  (add-hook 'js-mode-hook 'davazp/setup-node-modules-path))

(defun davazp/setup-node-modules-path ()
  (add-hook 'before-save-hook 'add-node-modules-path nil t))


(use-package macrostep
  :general
  (:keymaps 'emacs-lisp-mode-map
            "C-c RET" 'macrostep-expand))

(use-package forth-mode)

;; Misc

(use-package dockerfile-mode)
(use-package yaml-mode)
(use-package docker-compose-mode)
(use-package terraform-mode)
(use-package ninja-mode)
(use-package graphql-mode)
(use-package nix-mode)




;;
;; Themes
;;

(use-package zenburn-theme)
(use-package solarized-theme)
(use-package monokai-theme)
(use-package dracula-theme)
(use-package gruvbox-theme)
(use-package afternoon-theme)
(use-package badger-theme)
(use-package cherry-blossom-theme)
(use-package cyberpunk-theme)
(use-package grandshell-theme)
(use-package gruber-darker-theme)

(use-package hemisu-theme)
(use-package leuven-theme)

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(load custom-file t)

(use-package smart-mode-line
  :config
  (sml/setup))


;; We enable envrc after all other modes are enabled. This is recommended in its README
;;   https://github.com/purcell/envrc
;; to make its hook run _before_ other modes, giving the other modes like flycheck
;; the chance to see the defined environment variables.
(envrc-global-mode)

(let ((localfile (expand-file-name "local.el" user-emacs-directory)))
  (load localfile t))    


;; Custom global keybindings

(defun davazp/swap-last-two-buffers ()
  "Swap between the last current and the last visited buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun davazp/find-config ()
  "Edit config.org"
  (interactive)
  (find-file "~/.emacs.d/elisp/init.el"))

(general-define-key
 "C-;" 'davazp/swap-last-two-buffers
 "C-=" 'er/expand-region
 [remap text-scale-adjust] 'zoom-in/out)

(general-define-key
 :prefix "C-c"
 "a" '(org-agenda :which-key "Show org-mode agenda")
 "l" '(org-store-link :which-key "Store a org-mode link")
 "C" '(davazp/find-config :which-key "Open emacs config.")
 ;; "s" '(lsp-ivy-workspace-symbol :which-key "Symbol in workspace")
 ;; "S" '(lsp-ivy-workspace-symbol :which-key "Symbol in all active workspaces")
 "C-t" 'toggle-truncate-lines
 "C-r" '(ivy-resume :which-key "Resume last completion command")
 "C-e" '(flycheck-list-errors :which-key "List flycheck errors")
 "P" '(prodigy :which-key "Process manager"))

(general-define-key
 :prefix "C-c t"
 "" '(:ignore t :which-key "Toggle...")
 "b" '(presentation-mode :which-key "Toggle presentation mode")
 "f" '(focus-mode :which-key "Focus mode"))

(general-define-key
 :prefix "C-c o"
 "" '(:ignore t :which-key "Open...")
 "t" '(vterm :which-key "Open terminal"))

(general-define-key
 :prefix "C-c p"
 "" '(:ignore t :which-key "Projectile...")
 "f" '(projectile-find-file :which-key "Find a file in the project")
 "p" '(projectile-switch-project :which-key "Switch to project")
 "E" '(projectile-edit-dir-locals :which-key "Edit dir locals")
 "c" '(projectile-compile-project :which-key "Compile project")
 "s" '(counsel-projectile-git-grep :which-key "Search in repository")
 "x" '(:ignore t :which-key "Execute...")
 "x t" '(projectile-run-vterm :which-key "Execute terminal"))

(general-define-key
 :prefix "C-c m"
 "" '(:ignore t :which-key "Multiple cursors")
 "l" 'mc/edit-lines
 "a" 'mc/mark-all-dwim)

(general-define-key
 :prefix "C-c r"
 "" '(:ignore t :which-key "Restart Emacs")
 "r" '(restart-emacs :which-key "Restart Emacs"))


(provide 'init-core)
;;; init.el ends here
