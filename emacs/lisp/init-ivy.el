;;; init-ivy.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  David Vázquez Púa

;; Author: David Vázquez Púa <davazp@gmail.com>
;; Keywords:

;;; Commentary:

;;

;;; Code:

(use-package recentf
  :config
  (setq recentf-max-saved-items 1000))

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


(provide 'init-ivy)
;;; init-ivy.el ends here
