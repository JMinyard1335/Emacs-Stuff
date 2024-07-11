(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
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
(use-package autoinsert-setup
  :config
  ;; Change this path to the location of the cloned repo.
  (set-snippets-dir "c:/DevCornor/home/jachi/Emacs-Stuff/Snippets/"))
(use-package language-setup)
(use-package tools-setup
  :config
  (which-key-mode))
(use-package ligatures-setup)
(use-package copilot-setup
  :config
  (setq copilot--server-executable
	;; This was weried I could not get the copilot server to start because it emacs couldnt find it.
	;; This is the path where I found the server it might be different for you.
	".emacs.d/.cache/copilot/lib/node_modules/copilot-node-server/bin/copilot-node-server"))

