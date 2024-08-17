;;; language-setup.el --- Sets up treesitter and eglot  -*- lexical-binding: t; -*-

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

;; Sets up treesitter and eglot for language support.
;; Current Languages supported:
;; - Python
;; - C/C++
;; - Java

;;; Code:
(defun my/c-hook ()
  "This functions was created to enable options for both c and c++ modes."
  (setq c-default-style "k&r")
  (setq c-ts-mode-set-style "k&r")
  (setq c-basic-offset 4))

(defun my/python-mode-hook ()
  )

(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
	 ("<f5>" . eglot-recompile)
	 ("C-c r" . eglot-rename)
	 ("C-c a" . eglot-code-actions)))

(use-package c-ts-mode
  :ensure t
  :hook ((c-ts-mode . eglot-ensure))
  :mode (("\\.c\\'" . c-ts-mode)
	 ("\\.h\\'" . c-ts-mode)
	 ("\\.cpp\\'" . c-ts-mode)
	 ("\\.hpp\\'" . c-ts-mode)))

(use-package python
  :ensure t
  :hook ((python-ts-mode . eglot-ensure))
  :mode (("\\.py\\'" . python-ts-mode)))

(use-package java-ts-mode
  :ensure t
  :hook ((java-ts-mode . eglot-ensure))
  :mode (("\\.java\\'" . java-ts-mode)))

(use-package css-ts-mode
  :ensure t
  :mode (("\\.css\\'" . css-ts-mode)))


(provide 'language-setup)
;;; language-setup.el ends here
