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
;; A list of tools included in this package are:
;; - Company mode
;; - Rainbow delimeters
;; - Hide show minor mode
;; - Electric pair mode
;; - Flyspell mode
;; - Which key
;; - Nerd icons


;;; Code:
(defgroup my-tools nil
  "Custom group for tools."
  :group nil)

(defcustom my/merd-font-family "NFM"
  "The font family to use for nerd icons."
  :type 'string
  :group 'my-tools)

(defcustom my-company-mode-list '()
  "The company mode list is a list of mode-hooks to enable company mode in."
  :type 'list
  :group 'my-tools)

(defcustom my-rainbow-mode-list '()
  "The rainbow mode list is a list of mode-hooks to enable rainbow delimeters mode in."
  :type 'list
  :group 'my-tools)

(defcustom my-hs-mode-list '()
  "List of modes to enable the Hide show minor mode"
  :type 'list
  :group 'my-tools)

(defcustom my-electric-pair-mode-list '()
  "A List of all modes to enable electric pair mode in."
  :type 'list
  :group 'my-tools)

(defcustom my-flyspell-mode-list '()
  "A List of all modes to enable flyspell mode in."
  :type 'list
  :group 'my-tools)

(defun my-company-hook ()
  "Applys company mode to a list of hooks"
  (dolist (mode-hook my-company-mode-list)
    (add-hook mode-hook (lambda () (company-mode 1)))))

(defun my-rainbow-hook ()
  "Applys rainbow delimeter mode to a list of hooks"
  (dolist (mode-hook my-rainbow-mode-list)
    (add-hook mode-hook (lambda () (rainbow-delimiters-mode 1)))))

(defun my-hs-hook ()
  "Applys hide show minor mode to a list of hooks"
  (dolist (mode-hook my-hs-mode-list)
    (add-hook mode-hook (lambda () (hs-minor-mode 1)))))

(defun my-electric-pair-hook ()
  "Applys electric pair mode to a list of hooks"
  (dolist (mode-hook my-electric-pair-mode-list)
    (add-hook mode-hook (lambda () (electric-pair-mode 1)))))

(defun my-flyspell-hook ()
  "Applys flyspell to a list of hooks"
  (dolist (mode-hook my-flyspell-mode-list)
    (add-hook mode-hook (lambda () (flyspell-mode 1)))))

(defun my-tool-hook-setup ()
  "Sets up all the hooks for the tools."
  (my-company-hook)
  (my-rainbow-hook)
  (my-hs-hook)
  (my-electric-pair-hook)
  (my-flyspell-hook)
  (which-key-mode 1))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))

(use-package company-box
  :ensure t
  :hook
  (company-mode . company-box-mode)
  :config
  (setq company-box-color-icon t)
  (setq company-box-enable-icon t)
  (setq company-box-scrollbar nil))

(use-package rainbow-delimiters
  :ensure t
  :config
  (setq rainbow-delimiters-max-face-count 9)
  (setq rainbow-delimiters-outermost-only-face-count 0))

(use-package which-key
  :ensure t
  :config
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-max-width 0.35)
  (setq which-key-idle-delay 1))

(use-package nerd-icons
  :ensure t
  :custom					     
  (setq nerd-icons-font-family my/merd-font-family)
  (setq nerd-icons-color-icons t))

(provide 'tools-setup)
;;; tools-setup.el ends here
