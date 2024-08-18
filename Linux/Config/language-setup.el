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
(defun my-python-setup ()
  "Controls the python configuraion including things like indentation."
  (setq tab-width 4
	python-indent-offset 4))

(defun my-c++-setup ()
  "Controls the c++ configuraion including things like indentation."
  (setq
   ;;c-default-style "k&r"
   ;;c-basic-offset 4
   ;; c-ts-mode-indent-offset 4
   ;;c-ts-mode-indent-style "k&r"
   ))

(defun my-c-setup ()
  "Controls the c configuraion including things like indentation."
  (setq
   c-default-style "k&r"
   c-basic-offset 4
   c-ts-mode-indent-offset 4
   c-ts-mode-indent-style "k&r"))

(defun my-java-setup ()
  "Controls the java configuraion including things like indentation.")

(defun my-elisp-setup ()
  "Controls the elisp configuraion including things like indentation.")

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

(provide 'language-setup)
;;; language-setup.el ends here
