;;; init-tools.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2021  David Vázquez Púa

;; Author: David Vázquez Púa <davazp@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(use-package wgrep)

(use-package magit)
(use-package forge
  :after magit)

(use-package vterm)

;; Editor settings
(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package prodigy)

(use-package docker)


(provide 'init-tools)
;;; init-tools.el ends here
