;;; init-ui.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  David Vázquez Púa

;; Author: David Vázquez Púa <davazp@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

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

;; Display a clock in the modeline
(display-time-mode)

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

;;
;; Window management
;;

;; Use popup windows for some help buffers, so they don't stick around
;; and messup the window configuration.
(use-package popwin
  :config
  (popwin-mode))


;; Dim parenthesis
(use-package paren-face)
(global-paren-face-mode 1)


(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook '(dired-mode . all-the-icons-dired-mode))



(provide 'init-ui)
;;; init-ui.el ends here
