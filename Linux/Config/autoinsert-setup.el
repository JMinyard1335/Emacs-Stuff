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
;; My Template functions, These funtions are used to insert text templates into files. ----------------
(defun my-org-template ()
  "Creates a boilerplate for org files set up various settings."
    (let ((title (read-string "Title: "))
          (author (read-string "Author: " user-full-name)))
      (insert "#+TITLE: " title "\n")
      (insert "#+AUTHOR: " author "\n")
      (insert ":PROPERTIES:\n")
      (insert "#+LATEX_CLASS: article\n")
      (insert "#+STARTUP: overview\n")
      (insert "#+OPTIONS: toc:nil\n")
      (insert "#+OPTIONS: todo:nil\n")
      (insert "#+OPTIONS: H:6\n")
      (insert "#+OPTIONS: num:1\n")
      (insert "#+LATEX_HEADER: \\usepackage[margin=.75in]{geometry}\n")
      (insert "#+LATEX_HEADER_EXTRA: \\usepackage{tikz}\n")
      (insert "#+LATEX_HEADER_EXTRA: \\usepackage{graphicx}\n")
      (insert ":END:\n\n")))

(defun my-c-template ()
  "Creates a boilerplate for c files. Inserts some initial comments."
  (let ((author (read-string "Author: " user-full-name))
	(date (format-time-string "%Y-%m-%d"))
	(file (file-name-nondirectory (buffer-file-name))))
    (insert "/*\n")
    (insert " * @file " file "\n")
    (insert " * @brief Brief description of the file\n")
    (insert " * @author " author "\n")
    (insert " * @date " date "\n")
    (insert " */\n\n")))

;;------------------------------------------------------------------------------------------------

(defcustom my-snippets-dir "/Snippets/"
  "Directory containing yasnippet snippets."
  :type 'file
  :group 'autoinsert-setup)

(defun my-set-snippet-dirs (&optional directory)
  "Takes in a directory as a string and sets it as the snippets dir"
  (interactive)
  (if directory
      (let ((dir (concat (getenv "HOME") directory)))
	(message dir)
	(add-to-list 'yas-snippet-dirs dir))
    (add-to-list 'yas-snippet-dirs (concat (getenv "HOME") my-snippets-dir))))

(defun my/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (end-of-line)
  (yas-expand))

(use-package autoinsert
  :config
  (auto-insert-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (add-to-list 'auto-insert-alist
	       '("\\.org\\'" . my-org-template)))

(provide 'autoinsert-setup)
;;; autoinsert-setup.el ends here
