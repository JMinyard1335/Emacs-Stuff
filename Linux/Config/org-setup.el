;;; org-setup.el --- Contains Configuration for org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <jachi@DEVTABLET>
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

;; This holds configurations for org-mode and org-agenda. Basically if its org it can be found in this file.
;; This includes things like:
;; 1. Org-mode
;; 2. Org-Agenda
;; 3. Org-Babel
;; 4. Org-Super-Agenda
;; 5. Org-Modern

;;; Code:
(defun my/org-mode-setup ()
  (setq org-adapt-indentation t)
  (visual-line-mode 1))

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun my/org-font-setup ()
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(use-package org-capture)

(use-package org
  :init (setq inhibit-compacting-font-caches t)
  :hook (org-mode . my/org-mode-setup)
  :config
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (setq org-agenda-window-setup 'current-window)
  (setq org-log-into-drawer t)
  (setq org-log-done '('time))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")
	  (sequence "ASSIGNED(A)" "CURRENT(C)" "|" "TURNEDIN(T)")))
  (setq org-todo-keywords-for-agenda '("TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "ASSIGNED(A)" "CURRENT(C)"))
  (setq org-done-keywords-for-agenda '("DONE(d)" "CANCELED(c)" "TURNEDIN(T)"))
  (setq org-deadline-warning-days 7)
  (setq org-agenda-hide-tags-regexp ".*")
  (setq org-adapt-indentation t)
  (setq org-agenda-tags-column 40)
  (setq org-hide-drawer-startup t)
  (my/org-font-setup))

(use-package org-super-agenda
  :ensure t
  :hook (org-agenda-mode . org-super-agenda-mode))

(use-package org-modern
  :ensure t)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package olivetti
  :ensure t
  :hook
  (org-agenda-mode . olivetti-mode))

(use-package doct
  :ensure t
  :commands (doct))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . my/org-mode-visual-fill))

(provide 'org-setup)
;;; org-setup.el ends here
