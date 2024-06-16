;;; autoinsert-setup.el --- Sets up iniserting text templates into files  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <jachi@DEVTABLET>
;; Keywords: docs, convenience

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

;; This file will set up the autoinsert package and the yasnippet package. These packages allow the user to insert text templates into files. The autoinsert package will insert text templates into files when they are created, whereas the yasnippet package will insert text templates into files when the user types a certain keyword and presses the tab key.

;;; Code:
(defcustom snippets-dir ""
  "Directory containing yasnippet snippets."
  :type 'directory
  :group 'autoinsert-setup)

(defun set-snippets-dir (file)
  "Set the directory containing yasnippet snippets."
  (interactive "FSelect a path: ")
  (if (not (file-exists-p file))
      (find-file file))
  (setq snippets-dir file)
  (message "Yasnippets Directory set to: %s" snippets-dir))

;; (defun my/autoinsert-yas-expand()
;;   "Replace text in yasnippet template."
;;   (yas/expand-snippet (buffer-string) (point-min) (point-max)))

;; (defun my/autoinsert-yas-expand (key)
;;   "Expand a yasnippet for the current mode."
;;   (let ((snippet (yas-lookup-snippet key)))
;;     (when snippet
;;       (yas-expand-snippet snippet))))

(defun my/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (end-of-line)
  (yas-expand))

(use-package autoinsert
  :config
  (setq auto-insert-query nil)             
  (auto-insert-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (add-to-list 'yas-snippet-dirs "c:/DevCornor/home/jachi/Emacs-Stuff/Snippets/"))

  ;; (setq auto-insert-alist
  ;; 	'((("\\.\\(c\\|cpp\\|c++\\)\\'" . "c Source Files")
  ;; 	   . ["main.c" c-ts-mode my/autoinsert-yas-expand])))
(yas-global-mode 1)
(provide 'autoinsert-setup)
;;; autoinsert-setup.el ends here
