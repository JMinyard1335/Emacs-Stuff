;;; tools-setup.el --- Sets up misc tools            -*- lexical-binding: t; -*-

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

;; Set up tools misc tools for different things throughout the config.

;;; Code:

(use-package which-key
  :ensure t
  :custom
  (setq which-key-idle-delay 1))

(use-package nerd-icons
  :ensure t
  :custom					     
  (setq nerd-icons-font-family "NFM")
  (setq nerd-icons-color-icons t))

(use-package company
  :ensure t
  :hook
  (prog-mode . company-mode)
  :config (setq company-idle-delay 0.1
		company-minimum-prefix-length 1))

(use-package company-box
  :ensure t
  :hook
  (company-mode . company-box-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'tools-setup)
;;; tools-setup.el ends here
