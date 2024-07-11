(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(add-to-list 'load-path "~/Documents/Emacs-Stuff/Config/")
(use-package gui-setup
  :config
  (setq my-battery t)
  (setq my-font-italic t)
  (setq my-fullscreen t)
  (setup-gui))
(use-package modeline-setup)
(use-package autoinsert-setup)
(use-package language-setup)
(use-package tools-setup
  :config
  (which-key-mode))
(use-package ligatures-setup)
(use-package copilot-setup)

