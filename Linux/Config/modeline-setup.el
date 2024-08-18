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

(use-package doom-modeline
  :ensure t
  :config
  (setq column-number-mode t)
  (display-time)
  (setq doom-modeline-height 40
	doom-modeline-bar-width 8
	doom-modeline-hud nil
	doom-modeline-project-detection 'auto
	doom-modeline-buffer-name t
	doom-modeline-highlight-modified-buffer-name t
	doom-modeline-enable-word-count t
	doom-modeline-icon t
	doom-modeline-major-mode-icon t
	doom-modeline-major-mode-color-icon t
	doom-modeline-buffer-state-icon t
	doom-modeline-minor-modes nil
	doom-modeline-github t
	doom-modeline-battery-icon t
	doom-modeline-battery t
	doom-modeline-time t
	doom-modeline-time-icon t
	doom-modeline-time-live-icon t
	doom-modeline-buffer-encoding nil
	doom-modeline-display-misc-in-all-mode-lines nil
	doom-modeline-total-line-number t))

;; This is required to resize doom modeline so that it dosent block other buffers text.
(advice-add #'fit-window-to-buffer :before (lambda (&rest _) (redisplay t)))
(doom-modeline-mode)
(provide 'modeline-setup)
;;; modeline-setup.el ends here
