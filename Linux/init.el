(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
	                 		  ("org" . "https://orgmode.org/elpa/")
			                  ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
;; Load Packages Into The Config ----------------------------------------------------------------------------
(add-to-list 'load-path "~/Documents/Emacs-Stuff/Config/")
(use-package gui-setup
  :config
  (setq my-battery t)
  (setq my-font-italic t)
  (setq my-fullscreen t)
  (setup-gui))
(use-package modeline-setup)
(use-package autoinsert-setup
  :config
  (set-snippets-dir "~/Documents/Emacs-Stuff/Snippets"))
(use-package language-setup)
(use-package tools-setup
  :config
  (which-key-mode))
(use-package ligatures-setup)
(use-package copilot-setup)
(use-package org-setup)
;; Enable Built in Modes -------------------------------------------------------------------------------------


;; Enable Custom Keybings ------------------------------------------------------------------------------------
