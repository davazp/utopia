;;; init.el ---                                      -*- lexical-binding: t; -*-

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
(setq straight-use-package-by-default t)

(use-package general)

(require 'init-env)

;;
;; General
;;
(setq make-backup-files nil)

;; Mac adjustmenets
(setq mac-command-modifier 'meta)

(use-package restart-emacs)


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
(use-package diminish)

;;
;; Keybindings
;;
(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

(use-package org
  :straight (:type built-in))


;; Dim parenthesis
(use-package paren-face)
(global-paren-face-mode 1)


;;
;; Editor
;;
(use-package smartparens
  :init
  (require 'smartparens-config)
  (smartparens-global-strict-mode)
  :config
  (sp-use-paredit-bindings))

(use-package expand-region)
(use-package multiple-cursors)


;;
;; Ivy completion
;;

(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (ivy-mode))

(use-package counsel
  :diminish counsel-mode
  :init
  (counsel-mode))


(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

(use-package lsp-ui)
(use-package company
  :init
  (global-company-mode))

;;
;; Linting & Formatting
;;

(use-package flycheck
  :init
  (global-flycheck-mode))

(use-package format-all
  :commands (format-all-buffer format-all-mode)
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
		'(("JavaScript" prettier)
		  ("TypeScript" prettier))))


;;
;; Projects
;;

(use-package projectile
  :init
  (projectile-mode)
  :config
  (setq projectile-project-search-path '("~/Projects"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode))


;;
;; Tools
;;
(use-package magit)
(use-package vterm)

;; Dired
(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook '(dired-mode . all-the-icons-dired-mode))


;;
;; Languages
;;
(use-package typescript-mode
  :mode "\\.tsx\\'"
  :hook (typescript-mode . subword-mode)
  :hook (typescript-mode . lsp))

(use-package add-node-modules-path
  :hook (typescript-mode . add-node-modules-path)
  :hook (javascript-mode . add-node-modules-path))


(use-package macrostep
  :general
  (:keymap emacs-lisp-mode-map "C-c RET" 'macrostep-expand))

;;
;; Themes
;;
(use-package zenburn-theme)
(use-package solarized-theme)
(use-package monokai-theme)
(use-package dracula-theme)
(use-package gruvbox-theme)


(setq default-directory "~/")

(setq custom-file "~/.emacs-custom.el")
(when (file-exists-p custom-file)
  (load custom-file))


;; Custom global keybindings

(general-define-key
 "C-=" 'er/expand-region)

(general-define-key
 :prefix "C-c"
 "a" '(org-agenda :which-key "Show org-mode agenda")
 "t" '(vterm :which-key "Open terminal"))

(general-define-key
 :prefix "C-c m"
 "" '(:ignore t :which-key "Multiple cursors")
 "l" 'mc/edit-lines
 "a" 'mc/mark-all-dwim)

(general-define-key
 :prefix "C-c r"
 "" '(:ignore t :which-key "Restart Emacs")
 "r" '(restart-emacs :which-key "Restart Emacs"))

(general-define-key
 "C-c p" '(:keymap projectile-command-map :which-key "Project"))



(provide 'init)
