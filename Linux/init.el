(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(add-to-list 'custom-theme-load-path "~/Documents/Emacs-Stuff/Themes/")
(add-to-list 'load-path "~/Documents/Emacs-Stuff/Linux/Config/")
(setq warning-minimum-level :emergency)
;; Load Packages --------------------------------------------------------------------------------------
(use-package gui-setup
  :config
  (setq my-battery t)
  (setq my-font-italic t)
  (setq my-fullscreen t)
  (setq my-theme 'wombat)
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
	    (define-key copilot-mode-map (kbd "C-<right>") 'copilot-accept-completion-by-word)
	    (define-key copilot-mode-map (kbd "C-<down>") 'copilot-accept-completion-by-line)
	    (define-key copilot-mode-map (kbd "C-c n") 'copilot-next-completion)
	    (define-key copilot-mode-map (kbd "C-c p") 'copilot-previous-completion)))
  
;; Additional hooks ----------------------------------------------------------------------------------------

;; Enable hs-minor-mode, hs-minor-mode is uesd for code folding.
;; not great for languages like python that use whitespace as the delimeters
(dolist (mode-hook '(c++-mode-hook
		     c-ts-mode-hook
		     c-mode-hook
		     emacs-lisp-mode-hook
		     java-mode-hook))
  (add-hook mode-hook (lambda () (hs-minor-mode 1))))
;; Enables flyspell-mode, flyspell is used for spell checking
(dolist (mode-hook '(org-mode-hook
		     text-mode-hook))
  (add-hook mode-hook (lambda () (flyspell-mode 1))))
;; Enables Company-mode, Company mode is used for code suggestions and auto completions
;; This will also enable the company-box when company launches
(dolist (mode-hook '(c++-mode-hook
		     c++-ts-mode-hook
		     c-mode-hook
		     c-ts-mode-hook
		     java-mode-hook
		     java-ts-mode-hook
		     python-mode-hook
		     python-ts-mode-hook
		     emacs-lisp-mode-hook))
  (add-hook mode-hook (lambda () (company-mode 1))))
;; Enables rainbow-delimeters-mode, makes finding matching delimeters eaiser
(dolist (mode-hook '(c-mode-hook
		     c-ts-mode-hook
		     c++-mode-hook
		     c++-ts-mode-hook
		     emacs-lisp-mode-hook
		     java-mode-hook
		     java-ts-mode-hook))
  (add-hook mode-hook (lambda () (rainbow-delimiters-mode 1))))
;; Enables Copilot-mode, Have to download the server and login.
;; use the command copilot-install-server & copilot-login
(dolist (mode-hook '(org-mode-hook
		     c++-mode-hook
		     c++-ts-mode-hook
		     c-mode-hook
		     c-ts-mode-hook
		     java-mode-hook
		     java-ts-mode-hook
		     python-mode-hook
		     python-ts-mode-hook
		     emacs-lisp-mode-hook))
  (add-hook mode-hook (lambda () (copilot-mode 1))))
;; enable eglot for programming languages, eglot is used for language server protocols
(dolist (mode-hook '(c++-mode-hook
		     c++-ts-mode-hook
		     c-mode-hook
		     c-ts-mode-hook
		     java-mode-hook
		     java-ts-mode-hook
		     python-mode-hook
		     python-ts-mode-hook))
  (add-hook mode-hook (lambda () (eglot-ensure))))
;; Custom Hook for c and c++ modes, Sets different values for c/c++ files
;; This includes indention style, indention width, ect.
;; The 'my/c++-mode-hook' & `my/c-ts-mode-hook' functions are defined in 'language-setup.el'
(dolist (mode-hook '(c++-mode-hook
		     c-mode-hook))
  (add-hook mode-hook 'my/c++-mode-hook))
(dolist (mode-hook '(c-ts-mode-hook
		     c++-ts-mode-hook))
  (add-hook mode-hook 'my/c-ts-mode-hook))
;; Enables electric pair mode, electric pair mode is used for auto closing delimeters
(dolist (mode-hook '(c-mode-hook
		     c-ts-mode-hook
		     c++-mode-hook
		     c++-ts-mode-hook
		     emacs-lisp-mode-hook
		     java-mode-hook
		     java-ts-mode-hook
		     python-mode-hook))
  (add-hook mode-hook (lambda () (electric-pair-mode 1))))


