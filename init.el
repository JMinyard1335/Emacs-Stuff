(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(add-to-list 'load-path "~/Emacs-Stuff/Config/")
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
(use-package copilot-setup
  :config
  ;;(setq copilot--base-dir "~/.emacs.d/straight/repos/copilot.el/")
  (setq copilot--server-executable ".emacs.d/.cache/copilot/lib/node_modules/copilot-node-server/bin/copilot-node-server"))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ligature rainbow-delimiters company-box company which-key highlight-indent-guides yasnippet doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
