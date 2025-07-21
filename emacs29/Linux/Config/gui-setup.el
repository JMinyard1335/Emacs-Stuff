;;; gui-setup.el --- Modifies the Look and Feel of the emacs GUI  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <jachi@DEVTABLET>
;; Keywords: faces

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

;; This file will hold any customizations to the GUI of emacs. That includes variables to:
;; - Change the font size
;; - Change the font family
;; - Change the font Slant/Weight
;; - Change the theme
;; - Change the frame size (height, width) or make it fullscreen
;; - Enable/Disable the startup buffer
;; - Enable/Disable the menu bar
;; - Enable/Disable the tool bar
;; - Enable/Disable the scroll bar
;; - Enable/Disable the line numbers

;;; Code:
(defgroup my-gui nil
  "Customizations for the GUI of emacs."
  :group 'IDEmacs)

(defcustom my-font-name "VictorMonoNerdFontMono"
  "the name of the font family to be appilied to the default face."
  :type 'string
  :group 'my-gui)

(defcustom my-font-size 120
  "The size of the font to be applied to the default face."
  :type 'integer
  :group 'my-gui)

(defcustom my-font-italic nil
  "Whether the font should be italic or not."
  :type 'boolean
  :group 'my-gui)

(defcustom my-theme 'wombat
  "The theme to be applied to the default face."
  :type 'symbol
  :group 'my-gui)

(defcustom my-frame-width 100
  "The width of the frame to be applied to the default face."
  :type 'integer
  :group 'my-gui)

(defcustom my-frame-height 40
  "The height of the frame to be applied to the default face."
  :type 'integer
  :group 'my-gui)

(defcustom my-fullscreen nil
  "Whether the frame should be fullscreen or not."
  :type 'boolean
  :group 'my-gui)

(defcustom my-menu-bar nil
  "Whether the menu bar should be enabled or not."
  :type 'boolean
  :group 'my-gui)

(defcustom my-tool-bar nil
  "Whether the tool bar should be enabled or not."
  :type 'boolean
  :group 'my-gui)

(defcustom my-scroll-bar nil
  "Whether the scroll bar should be enabled or not."
  :type 'boolean
  :group 'my-gui)

(defcustom my-line-numbers t
  "Whether the line numbers should be enabled or not for programing modes."
  :type 'boolean
  :group 'my-gui)

(defcustom my-tab-line t
  "Whether the tab line should be enabled or not."
  :type 'boolean
  :group 'my-gui)

(defcustom my-startup-buffer nil
  "Whether the startup buffer should be displayed or not.
This buffer is the one that emcas shows on default startup.
it contains things like a tutorial and some basic info."
  :type 'boolean
  :group 'my-gui)

(defun my-set-font ()
  "Set the font of the default face."
  (set-face-attribute 'default nil :font my-font-name :height my-font-size :italic my-font-italic))

(defun my-set-theme ()
  "Set the theme of the default face."
  (load-theme my-theme t))

(defun my-set-frame-size ()
  "Set the size of the frame."
  (add-to-list 'default-frame-alist (cons 'width my-frame-width))
  (add-to-list 'default-frame-alist (cons 'height my-frame-height)))

(defun my-set-fullscreen ()
  "Set the frame to be fullscreen."
  (if my-fullscreen
      (add-to-list 'default-frame-alist (cons 'fullscreen 'maximized))
    (my-set-frame-size)))

(defun my-set-misc-display ()
  "Set the display of the time, battery, startup buffer, menu bar, tool bar, scroll bar, line numbers, and tab line."
  (if my-startup-buffer
      (setq inhibit-startup-screen nil)
    (setq inhibit-startup-screen t))
  (if my-menu-bar
      (menu-bar-mode 1)
    (menu-bar-mode -1))
  (if my-tool-bar
      (tool-bar-mode 1)
    (tool-bar-mode -1))
  (if my-scroll-bar
      (scroll-bar-mode 1)
    (scroll-bar-mode -1))
  (if my-line-numbers
      (dolist (mode '(prog-mode-hook))
	(add-hook mode (lambda () (display-line-numbers-mode 1)))))
  (if my-tab-line
      (global-tab-line-mode 1)
    (global-tab-line-mode -1)))

(defun setup-gui ()
  "Set up the GUI of emacs."
  (my-set-font)
  (my-set-theme)
  (my-set-fullscreen)
  (my-set-misc-display))

(provide 'gui-setup)
;;; gui-setup.el ends here
