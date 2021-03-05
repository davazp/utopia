;;; init.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  David

;; Author: David <davazp@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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


(use-package general)

;;
;; General
;;
(setq make-backup-files nil)

;; Mac adjustmenets
(setq mac-command-modifier 'meta)

(use-package restart-emacs)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


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
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package org
  :straight (:type built-in))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))


;; Dim parenthesis
(use-package paren-face)
(global-paren-face-mode 1)


;;
;; Editor
;;
(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-strict-mode)
  (sp-use-paredit-bindings))

(use-package expand-region)
(use-package multiple-cursors)

(use-package wgrep)


;;
;; Ivy completion
;;

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode))

(use-package counsel
  :diminish counsel-mode
  :config
  (counsel-mode))


;;
;; Language server supportt
;;

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l"))

(use-package lsp-ui)
(use-package company
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
		'(("JavaScript" prettier)
		  ("TypeScript" prettier))))


;;
;; Projects
;;

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-project-search-path '("~/Projects"))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :config
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


;; Editor settings
(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; Highlight TODO, FIXME and NOTE in comments.
(use-package hl-todo
  :config
  (global-hl-todo-mode 1))

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
 "t" '(vterm :which-key "Open terminal")
 "C-r" '(ivy-resume :which-key "Resume last completion command")
 "C-e" '(flycheck-list-errors :which-key "List flycheck errors"))

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
;;; init.el ends here
