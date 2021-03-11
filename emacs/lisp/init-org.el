;;; init-org.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  David Vázquez Púa

;; Author: David Vázquez Púa <davazp@gmail.com>
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

(use-package org
  :config
  (setq org-odd-levels-only nil)
  (setq org-startup-indented t)
  (setq org-special-ctrl-k t)
  (setq org-special-ctrl-a/e t)
  ;; Error if trying to delete some text that is invisible.  Note that
  ;; this this does not affect killing a full subtree when folded
  ;; (thanks to `org-special-ctrl-k').
  (setq org-catch-invisible-edits 'show-and-error)

  (setq org-enforce-todo-dependencies t)

  (require 'org-tempo)
  (require 'org-clock)
  (require 'ox-md)

  ;; (setq org-clock-idle-time nil)
  ;; (setq org-log-done 'time)
  ;; (setq org-clock-auto-clock-resolution nil)
  ;; (setq org-clock-continuously nil)
  ;; (setq org-clock-persist t)
  ;; (setq org-clock-in-switch-to-state "STARTED")
  ;; (setq org-clock-in-resume nil)
  ;; (setq org-show-notification-handler 'message)
  ;; (setq org-clock-report-include-clocking-task t))

  ;; (org-clock-persistence-insinuate)

  ;; (setq org-refile-use-outline-path 'file)
  ;; (setq org-outline-path-complete-in-steps nil)
  ;; (setq org-refile-allow-creating-parent-nodes 'confirm)
  ;; (setq org-refile-targets nil)

  )

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))


(provide 'init-org)
;;; init-org.el ends here
