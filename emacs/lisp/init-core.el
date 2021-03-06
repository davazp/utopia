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

;; Allow to diminish minor modes so they do not clutter the modeline
(use-package diminish
  :diminish eldoc-mode)

;; Declare it early to prevent having a mixed installation.
(use-package org)

;;
;; General
;;
(setq make-backup-files nil)
(setq create-lockfiles nil)


;; Ensure that M-a and M-e will stop after a dot, even without two
;; spaces after it.
(setq sentence-end-double-space nil)

(setq-default indent-tabs-mode nil)

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

(use-package saveplace
  :straight (:type built-in)
  :config
  (save-place-mode))

;;
;; Additional modules
;;
(require 'init-ivy)
(require 'init-editor)
(require 'init-ui)
(require 'init-tools)
(require 'init-langs)
(require 'init-org)

;;
;; Projects
;;

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-project-search-path '("~/Projects"))
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))


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


(defun davazp/swap-last-two-buffers ()
  "Swap between the last current and the last visited buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(general-define-key
 "C-;" 'davazp/swap-last-two-buffers
 "C-=" 'er/expand-region
 [remap text-scale-adjust] 'zoom-in/out)

(general-define-key
 :keymaps '(lsp-mode-map)
 :prefix "C-c"
 "r" '(lsp-rename :which-key "Rename")
 "a" '(lsp-execute-code-action :which-key "Code action")
 "g" (general-simulate-key "s-l g" :which-key "Go to..."))

(general-define-key
 :prefix "C-c"
 "a" '(org-agenda :which-key "Show org-mode agenda")
 "l" '(org-store-link :which-key "Store a org-mode link")
 "s" '(lsp-ivy-workspace-symbol :which-key "Symbol in workspace")
 "S" '(lsp-ivy-workspace-symbol :which-key "Symbol in all active workspaces")
 "C-r" '(ivy-resume :which-key "Resume last completion command")
 "C-e" '(flycheck-list-errors :which-key "List flycheck errors")
 "P" '(prodigy :which-key "Process manager"))

(general-define-key
 :prefix "C-c t"
 "" '(:ignore t :which-key "Toggle...")
 "t" '(toggle-truncate-lines :which-key "Toggle Truncate lines")
 "b" '(presentation-mode :which-key "Toggle presentation mode")
 "f" '(focus-mode :which-key "Focus mode"))

(general-define-key
 :prefix "C-c o"
 "" '(:ignore t :which-key "Open...")
 "t" '(vterm :which-key "Open terminal"))

(general-define-key
 :prefix "C-c p"
 "" '(:ignore t :which-key "Projectile...")
 "d" '(projectile-dired :which-key "Open root directory")
 "f" '(projectile-find-file :which-key "Find a file in the project")
 "p" '(projectile-switch-project :which-key "Switch to project")
 "E" '(projectile-edit-dir-locals :which-key "Edit dir locals")
 "c" '(projectile-compile-project :which-key "Compile project")
 "k" '(projectile-kill-buffers :which-key "Kill buffers")
 "r" '(projectile-run-project :which-key "Run project")
 "s" '(counsel-projectile-rg :which-key "Search in repository")
 "t" '(projectile-run-vterm :which-key "Execute terminal"))

(general-define-key
 :prefix "C-c m"
 "" '(:ignore t :which-key "Multiple cursors")
 "l" 'mc/edit-lines
 "a" 'mc/mark-all-dwim)

(general-define-key
 :prefix "C-c R"
 "" '(:ignore t :which-key "Restart Emacs")
 "R" '(restart-emacs :which-key "Restart Emacs"))


(provide 'init-core)
;;; init.el ends here
