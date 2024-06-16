;;; copilot-setup.el --- Sets up copilot for use in emacs.  -*- lexical-binding: t; -*-

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

;; This packages sets up copilot for use in emacs. It can be used in both programming and non programming modes.

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :hook
  (c-ts-mode . copilot-mode)
  (cpp-ts-mode . copilot-mode)
  (emacs-lisp-mode .copilot-mode)
  (org-mode . copilot-mode)
  (python-ts-mode . copilot-mode)
  (java-ts-mode . copilot-mode)
  :bind
  (:map copilot-completion-map
	("C-<return>" . copilot-accept-completion)
	("C-<tab>" . copilot-accept-completion-by-word))
  :config
  (setq copilot-indent-offset-warning-disable t))

(defun darken-color (color percent)
  "Darken a COLOR by a certain PERCENT."
  (let ((rgb (color-name-to-rgb color))
        (amount (/ percent 100.0)))
    (apply 'color-rgb-to-hex
           (mapcar (lambda (component)
                     (max 0.0 (* component (- 1 amount))))
                   rgb))))

(let ((default-color (face-attribute 'default :foreground)))
  (custom-set-faces
   `(copilot-overlay-face ((t (:foreground ,(darken-color default-color 50) :inherit default :slant italic))))))

(provide 'copilot-setup)
;;; copilot-setup.el ends here
