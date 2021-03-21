;;; init-langs.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2021  David Vázquez Púa

;; Author: David Vázquez Púa <davazp@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

;;
;; Language Server Protocol support
;;

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  ;; This is the default. But let's be explicit. We'll bind most
  ;; common commands under C-c anywa.
  :init
  (setq lsp-keymap-prefix "s-l"))

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
                  ("SQL" (pgformatter "-s2"))
                  ("Rust" rustfmt))))


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


;; Emacs Lisp

(use-package macrostep
  :general
  (:keymaps 'emacs-lisp-mode-map
            "C-c RET" 'macrostep-expand))


;; Rust

(use-package rust-mode
  :hook (rust-mode . lsp))


;; Other languages

(use-package forth-mode)
(use-package dockerfile-mode)
(use-package yaml-mode)
(use-package docker-compose-mode)
(use-package terraform-mode)
(use-package ninja-mode)
(use-package graphql-mode)
(use-package nix-mode)
(use-package glsl-mode)


(provide 'init-langs)
;;; init-lang-misc.el ends here
