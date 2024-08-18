;;; modeline-setup.el --- Sets up the doom modeline  -*- lexical-binding: t; -*-

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

;; Sets up the doom modeline.

;;; Code:
(defgroup my-modeline nil "Settings for the modeline.")


(defcustom my-modeline-height 40
  "The height of the modeline."
  :type 'integer
  :group 'my-modeline)

(defcustom my-modeline-bar-width 8
  "The width of the bar in the modeline.
little strip on the left."
  :type 'integer
  :group 'my-modeline)

(defcustom my-modeline-time t
  "Whether to display the time in the modeline.
When non nil, the following variables are set:
`doom-modeline-time'
`doom-modeline-time-icon'
`doom-modeline-time-live-icon'
`display-time'"
  :type 'boolean
  :group 'my-modeline)

(defcustom my-modeline-battery nil
  "Whether to display the battery in the modeline.
When non nil, the following variables are set:
`doom-modeline-battery'
`doom-modeline-battery-icon'"
  :type 'boolean
  :group 'my-modeline)

(defcustom my-modeline-display-buffer t
  "Whether to display the buffer name in the modeline.
When non nil, the following variables are set:
`doom-modeline-buffer-name'
`doom-modeline-highlight-modified-buffer-name'
`doom-modeline-buffer-state-icon'
`doom-modeline-major-mode-icon'
`doom-modeline-major-mode-color-icon'"
  :type 'boolean
  :group 'my-modeline)

(defcustom my-modeline-content-info t
  "Whether to display info about the buffers content.
When non nil, the following variables are set:
`doom-modeline-enable-word-count'
`doom-modeline-total-line-number'
`column-number-mode'
`line-number-mode'
"
  :type 'boolean
  :group 'my-modeline)


(defun my-modeline-time-setup ()
  (if my-modeline-time
      (progn (display-time-mode 1)
	     (setq doom-modeline-time t
		   doom-modeline-time-icon t))
    (progn(display-time-mode -1)
	  (setq doom-modeline-time nil
		doom-modeline-time-icon nil))))


(defun my-modeline-battery-setup ()
  (if my-modeline-battery
      (progn (display-battery-mode 1)
	     (setq doom-modeline-battery t
		   doom-modeline-battery-icon t))
    (progn(display-battery-mode -1)
	  (setq doom-modeline-battery nil
		doom-modeline-battery-icon nil))))

(defun my-modeline-display-buffer-setup ()
  (if my-modeline-display-buffer
      (setq doom-modeline-buffer-name t
	    doom-modeline-highlight-modified-buffer-name t
	    doom-modeline-buffer-state-icon t
	    doom-modeline-major-mode-icon t
	    doom-modeline-major-mode-color-icon t)
    (setq doom-modeline-buffer-name nil
	  doom-modeline-highlight-modified-buffer-name nil
	  doom-modeline-buffer-state-icon nil
	  doom-modeline-major-mode-icon nil
	  doom-modeline-major-mode-color-icon nil)))

(defun my-modeline-content-info-setup ()
  (if my-modeline-content-info
      (setq doom-modeline-enable-word-count t
	    doom-modeline-total-line-number t
	    doom-modeline-github t
	    column-number-mode t
	    line-number-mode t)
    (setq doom-modeline-enable-word-count nil
	  doom-modeline-total-line-number nil
	  doom-modeline-github nil
	  column-number-mode nil
	  line-number-mode nil)))

(defun my-modeline-setup ()
  "Set up the modeline."
  (setq doom-modeline-height my-modeline-height
	doom-modeline-bar-width my-modeline-bar-width
	doom-modeline-project-detection 'auto
	doom-modeline-icon t)
  (my-modeline-time-setup)
  (my-modeline-battery-setup)
  (my-modeline-display-buffer-setup)
  (my-modeline-content-info-setup))


(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-minor-modes nil
	doom-modeline-buffer-encoding nil))

;; This is required to resize doom modeline so that it dosent block other buffers text.
(advice-add #'fit-window-to-buffer :before (lambda (&rest _) (redisplay t)))
(doom-modeline-mode)

(provide 'modeline-setup)
;;; modeline-setup.el ends here
