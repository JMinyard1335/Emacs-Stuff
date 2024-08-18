(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(add-to-list 'custom-theme-load-path "~/Emacs-Stuff/Themes/")
(add-to-list 'load-path "~/Emacs-Stuff/Linux/Config/")

(setq warning-minimum-level :emergency)

;; Load Packages --------------------------------------------------------------------------------------

(use-package gui-setup
  :config
  (setq my-battery t
	my-font-italic t
	my-fullscreen t
	my-theme 'wombat)
  (setup-gui))

(use-package modeline-setup
  :config
  (setq my-modeline-battery t
	my-modeline-time t
	my-modeline-display-buffer t
	my-modeline-content-info t)
  (my-modeline-setup))

(use-package autoinsert-setup
  :config
  (set-snippets-dir "~/Emacs-Stuff/Snippets")
  (yas-reload-all))

(use-package language-setup
  :config
  (setq my-eglot-ensure-list
	'(c++-mode-hook
	  c++-ts-mode-hook
	  c-mode-hook
	  c-ts-mode-hook
	  java-mode-hook
	  java-ts-mode-hook
	  python-mode-hook
	  python-ts-mode-hook))
  (my-eglot-ensure))

(use-package tools-setup
  :config
  (setq my-company-mode-list '(prog-mode-hook)
	my-electric-pair-mode-list '(prog-mode-hook)
	my-rainbow-mode-list '(prog-mode-hook org-mode-hook)
	my-flyspell-mode-list '(text-mode-hook org-mode-hook)
	my-hs-mode-list '(prog-mode-hook))
  (my-tool-hook-setup))

(use-package ligatures-setup)

(use-package copilot-setup
  :config
  (setq my-copilot-mode-list
	'(prog-mode-hook
	  org-mode-hook))
  (my-copilot-hook))

(use-package org-setup
       :config
       (setq org-directory "~/Emacs-Stuff/Org/")
       (my-org-agenda-files '("school.org" "journal.org" "todo.org"))
       (setq org-capture-templates (doct (list (my-school-capture))))
       (my-org-agenda)
       (my-org-tags)
       (my-org-logging)
       (my-org-faces)
       (my-org-todo))

;; Set Up Keybinds -----------------------------------------------------------------------------------------

(add-hook 'hs-minor-mode-hook
	  (lambda ()
	    (define-key hs-minor-mode-map (kbd "C-;") 'hs-hide-block)
	    (define-key hs-minor-mode-map (kbd "C-'") 'hs-show-block)
	    (define-key hs-minor-mode-map (kbd "C-:") 'hs-hide-all)
	    (define-key hs-minor-mode-map (kbd "C-\"") 'hs-show-all)))

(add-hook 'flyspell-mode-hook
	  (lambda ()
	    (define-key flyspell-mode-map (kbd "C-s c") 'flyspell-correct-word-before-point)
	    (define-key flyspell-mode-map (kbd "C-s b") 'flyspell-buffer)
	    (define-key flyspell-mode-map (kbd "C-s a") 'ispell-buffer)))

(add-hook 'company-mode-hook
	  (lambda ()
	    (define-key company-active-map (kbd "C-n") 'company-select-next)
	    (define-key company-active-map (kbd "C-p") 'company-select-previous)
	    (define-key company-active-map (kbd "C-g") 'company-abort)))

(add-hook 'copilot-mode-hook
	  (lambda ()
	    (define-key copilot-mode-map (kbd "C-<return>") 'copilot-complete)
	    (define-key copilot-mode-map (kbd "C-s-<right>") 'copilot-accept-completion-by-word)
	    (define-key copilot-mode-map (kbd "C-s-<down>") 'copilot-accept-completion-by-line)
	    (define-key copilot-mode-map (kbd "C-c n") 'copilot-next-completion)
	    (define-key copilot-mode-map (kbd "C-c p") 'copilot-previous-completion)))
