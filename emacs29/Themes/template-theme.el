;;; template-theme.el --- A template to make creating themes faster.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jachin

;; Author: Jachin <jachin@CodeHaven>
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

;; I Want this template to have several functions that set different packages in the theme.
;; By this I mean a function that will set all the fontlock-faces, the org-faces...and so on.
;; Part of this will help as I can then define list of colors that can be passed in as arguments to the functions.
;; This will allow making different styles for different mode more accessiable.
;; The Packages I need to implament functions for are the following
;; 1. Default-Faces
;; 2. Tab-bar-Faces
;; 3. Linenumber-Faces
;; 4. Fontlock-Faces
;; 5. Modeline-Faces
;; 6. Doom-Modeline-Faces
;; 7. Modeline-Battery-Faces
;; 8. Modeline-Buffer-Info-Faces
;; 9. Modeline-Debug-Faces
;; 10. Modeline-Boon-Faces
;; 11. 
;; 12. 
;; 13. 
;; 14. 
;; 15. 
;; 16. 

;;; Code:
(deftheme template "Used to create other themes")

;; Misc Functions -------------------------------------------------------------------------------------
(defun make-face (name &rest args)
  "This function is used to simplify the syntax of setting face attributes"
  (list name `((t, args))))

;; Color List -----------------------------------------------------------------------------------------
(setq basic-color-list '( :white "#FFFFFF"
			  :black "#000000"
			  :red "#FF0000"
			  :blue "#0000FF"
			  :green "#008000"
			  :yellow "#FFFF00"(setq org-colors '())
(setq delimeters-color '())
			  :orange "#FFA500"
			  :purple "#800080"
			  :cyan "#00FFFF"
			  :magenta "#FF00FF"
			  :grey "#808080"
			  :brown "#A52A2A"))

;; System Faces ---------------------------------------------------------------------------------------
(defun set-default-faces (colors)
  (let ((fg (plist-get colors :white))
	(fg-accent ())
	(bg (plist-get colors :black))
	(bg-accent ())
	(accent (plist-get colors :purple))
	(success (plist-get colors :green))
	(warnings (plist-get colors :orange))
	(errors (plist-get colors :red))
	(highlights (plist-get colors :yellow)))
    (make-face 'default :background bg :foreground fg :italic t)
    (make-face 'bold :weight bold)
    (make-face 'italic :slant italic)
    (make-face 'bold-italic :weight bold :slant italic) 
    (make-face 'underline :underline t)
    (make-face 'shadow ) ;; make blend slightly into the background
    (make-face 'highlight :background highlights :foreground bg)
    (make-face 'border )
    (make-face 'vertical-border )
    (make-face 'fringe )
    (make-face 'region )
    (make-face 'custom-face-tag )
    (make-face 'custom-state )
    (make-face 'link )
    (make-face 'link-visited )
    (make-face 'cursor )
    (make-face 'error )
    (make-face 'warning )
    (make-face 'success )
    (make-face 'secondary-selection ))

(defun set-tab-bar-faces (colors)
  (make-face 'tab-line )
  (make-face 'tab-line-close-highlight)
  (make-face 'tab-line-tab-current )
  (make-face 'tab-line-tab-inactive )
  (make-face 'tab-bar )
  (make-face 'tab-bar-tab )
  (make-face 'tab-bar-tab-inactive ))

(defun set-linenumber-faces (colors)
  (make-face 'line-number )
  (make-face 'line-number-current-line ))

(defun set-fontlock-faces (colors)
  (make-face 'font-lock-warning-face )
  (make-face 'font-lock-function-name-face )
  (make-face 'font-lock-variable-name-face )
  (make-face 'font-lock-variable-use-face )
  (make-face 'font-lock-keyword-face )
  (make-face 'font-lock-comment-face )
  (make-face 'font-lock-comment-delimiter-face )
  (make-face 'font-lock-type-face )
  (make-face 'font-lock-constant-face )
  (make-face 'font-lock-builtin-face )
  (make-face 'font-lock-preprocessor-face )
  (make-face 'font-lock-string-face )
  (make-face 'font-lock-doc-face )
  (make-face 'font-lock-doc-markup-face )
  (make-face 'font-lock-negation-char-face )
  (make-face 'font-lock-escape-face )

;; Modeline Faces -------------------------------------------------------------------------------------
(defun set-modeline-faces (colors)
  (make-face 'mode-line )
  (make-face 'mode-line-inactive )
  (make-face 'mode-line-highlight )
  (make-face 'mode-line-buffer-id ))

(defun set-doom-modeline-faces (colors)
  (make-face 'doom-modeline :inherit 'nerd-icons-faces ) ;; dosent seem to be the whole bar
  (make-face 'doom-modeline-bar :inherit 'mode-line-highlight) ;; little sliver on the left side of the bar
  (make-face 'doom-modeline-bar-inactive :inherit 'mode-line-inactive)
  (make-face 'doom-modeline-emphasis )
  (make-face 'doom-modeline-warning )
  (make-face 'doom-modeline-urgent )
  (make-face 'doom-modeline-info )
  (make-face 'doom-modeline-highlight :inherit 'mode-line-highlight)
  (make-face 'doom-modeline-time )
  (make-face 'doom-modeline-host :italic t) ;; add a forground of choice
  (make-face 'doom-modeline-god :inherit 'doom-modeline-info)
  (make-face 'doom-modeline-notification :inherit 'doom-modeline-warning)
  (make-face 'doom-modeline-overwrite :inherit 'doom-modeline-urgent)
  (make-face 'doom-modeline-panel :inherit 'doom-modeline-highlight)
  (make-face 'doom-modeline-ryo :inherit 'doom-modeline-info)
  (make-face 'doom-modeline-unread-number :inherit 'doom-modeline)
  (make-face 'doom-modeline--icon )
  (make-face 'doom-modeline-fly-insert-state :inherit '(doom-modeline font-lock-keyword-face))
  (make-face 'doom-modeline-fly-normal-state :inherit 'doom-modeline-info)
  (make-face 'doom-modeline-input-method :inherit 'doom-modeline-emphasis)
  (make-face 'doom-modeline-input-method-alt :inherit '(doom-modelinie font-lock-doc-face))
  (make-face 'doom-modeline-project-dir :inherit 'mode-line)
  (make-face 'doom-modeline-project-parent-dir :inherit 'doom-modeline-project-dir)
  (make-face 'doom-modeline-project-root-dir 'doom-modeline-project-dir))

(defun set-modeline-batter-faces (colors)
  (make-face 'doom-modeline-battery-normal :inherit 'doom-modeline)
  (make-face 'doom-modeline-battery-charging :foreground green)
  (make-face 'doom-modeline-battery-critical :inherit 'doom-modeline-urgent)
  (make-face 'dooam-modeline-battery-error :inherit 'doom-modeline-urgent)
  (make-face 'doom-modeline-battery-full :inherit 'doom-modeline-info)
  (make-face 'doom-modeline-battery-warning :inherit 'doom-modeline-warning))

(defun set-modeline-buffer-info-faces (colors)
  (make-face 'doom-modeline-buffer-file :inherit '(doom-modeline mode-line-buffer-id) :bold t)
  (make-face 'doom-modeline-buffer-major-mode )
  (make-face 'doom-modeline-minor-modes :inherit 'font-lock-doc-face)
  (make-face 'doom-modeline-buffer-modified :inherit 'doom-modeline-warning :bold t)
  (make-face 'doom-modeline-buffer-path :inherit 'doom-modeline-emphasis :bold t)
  (make-face 'doom-modeline-buffer-timemachine ))

(defun set-modeline-debug-faces (colors)
  (make-face 'doom-modeline-compilation :inherit 'doom-modeline-warning)
  (make-face 'doom-modeline-debug :inherit '(doom-modeline font-lock-doc-face))
  (make-face 'doom-modeline-debug-visual :inherit 'doom-modeline)
  (make-face 'doom-modeline-lsp-error :inherit 'doom-modeline-urgent)
  (make-face 'doom-modeline-lsp-running :inherit '(doom-mode-line compilation-mode-line-run))
  (make-face 'doom-modeline-lsp-success :inherit 'doom-modeline-info)
  (make-face 'doom-modeline-lsp-warning :inherit 'doom-modeline-warning)
  (make-face 'doom-modeline-repl-success :inherit 'doom-modeline-info)
  (make-face 'doom-modeline-repl-warning :inherit 'doom-modeline-warning))

(defun set-modeline-boon-faces (colors)
  (make-face 'doom-modeline-boon-command-state :inherit 'doom-modeline-info)
  (make-face 'doom-modeline-boon-insert-state :inherit '(doom-modeline font-lock-keyword-face))
  (make-face 'doom-modeline-boon-off-state :inherit '(mode-line doom-modeline))
  (make-face 'doom-modeline-boon-special-state :inherit '(doom-modeline font-lock-builtin-face)))

;; Org Faces ------------------------------------------------------------------------------------------
(defun set-org-agenda-faces (colors)
  (make-face 'org-agenda-structure )
  (make-face 'org-agenda-calendar-event )
  (make-face 'org-agenda-calendar-sexp )
  (make-face 'org-agenda-clocking :inherit 'secondary-selection)
  (make-face 'org-agenda-column-dateline :inherit 'org-column)
  (make-face 'org-agenda-current-time :inherit 'org-time-grid :italic t)
  (make-face 'org-agenda-date )
  (make-face 'org-agenda-date-today )
  (make-face 'org-agenda-date-weekend :inherit 'org-agenda-date)
  (make-face 'org-agenda-diary :inherit 'default)
  (make-face 'org-agenda-dimmed-todo-face )
  (make-face 'org-agenda-done )
  (make-face 'org-agenda-filter-category :inherit 'mode-line)
  (make-face 'org-agenda-filter-effort :inherit 'mode-line)
  (make-face 'org-agenda-filter-regexp :inherit 'mode-line)
  (make-face 'org-agenda-filter-tags :inherit 'mode-line)
  (make-face 'org-agenda-restriction-lock ))

(defun set-org-codeblock-faces (colors)
  (make-face 'org-block :extend t :background grey10)
  (make-face 'org-block-begin-line :extend t :background grey20 :foreground grey80)
  (make-face 'org-block-end-line :inherit 'org-block-begin-line))

(defun set-org-check-boxes-faces (colors)
  (make-face 'org-checkbox )
  (make-face 'org-checkbox-statistics-done :inherit 'org-done)
  (make-face 'org-checkbox-statistics-todo :inherit 'org-todo))

(defun set-org-time-faces (colors)
  (make-face 'org-clock-overlay)
  (make-face 'org-mode-line-clock :inherit 'mode-line)
  (make-face 'org-mode-line-clock-overrun :inherit 'mode-line :background red)
  (make-face 'org-date :underline t)
  (make-face 'org-date-selected :inverse-video t)
  (make-face 'org-scheduled :foreground purple)
  (make-face 'org-scheduled-previously )
  (make-face 'org-scheduled-today )
  (make-face 'org-upcoming-deadline)
  (make-face 'org-upcoming-distant-deadline)
  (make-face 'org-time-grid :italic t))

(defun set-org-headers-faces (colors)
  (make-face 'org-level-1 :foreground pastel-blue :height 1.30)
  (make-face 'org-level-2 :foreground pastel-green :height 1.28)
  (make-face 'org-level-3 :foreground pastel-purple :height 1.26)
  (make-face 'org-level-4 :foreground pastel-red :height 1.24)
  (make-face 'org-level-5 :foreground pastel-blue :height 1.22)
  (make-face 'org-level-6 :foreground pastel-green :height 1.20)
  (make-face 'org-level-7 :foreground pastel-purple :height 1.18)
  (make-face 'org-level-8 :foreground pastel-red :height 1.16))

;; Tool Faces -----------------------------------------------------------------------------------------
(defun set-delimeter-faces (colors)
  (make-face 'rainbow-delimiters-depth-1-face :foreground pastel-blue)
  (make-face 'rainbow-delimiters-depth-2-face :foreground pastel-beige)
  (make-face 'rainbow-delimiters-depth-3-face :foreground pastel-green)
  (make-face 'rainbow-delimiters-depth-4-face :foreground pastel-purple)
  (make-face 'rainbow-delimiters-depth-5-face :foreground pastel-red)
  (make-face 'rainbow-delimiters-depth-6-face :foreground pastel-blue)
  (make-face 'rainbow-delimiters-depth-7-face :foreground pastel-beige)
  (make-face 'rainbow-delimiters-depth-8-face :foreground pastel-green)
  (make-face 'rainbow-delimiters-depth-9-face :foreground pastel-purple)
  (make-face 'rainbow-delimiters-mismatched-face :foreground red)
  (make-face 'rainbow-delimiters-unmatched-face :foreground orange))

(defun set-company-faces (colors)
  (make-face 'company-echo :inherit 'default)
  (make-face 'company-echo-common :inherit 'company-echo)
  (make-face 'company-preview :inherit '(company-tooltip-selection company-tooltip))
  (make-face 'company-preview-common :inherit 'company-tooltip-common-selection)
  (make-face 'company-preview-search :inherit 'company-tooltip-common-selection)
  (make-face 'company-template-field )
  (make-face 'company-tooltip )
  (make-face 'company-tooltip-annotation )
  (make-face 'company-tooltip-annotation-selection )
  (make-face 'company-tooltip-common )
  (make-face 'company-tooltip-common-selection )
  (make-face 'company-tooltip-deprecated :strike-through t :foreground red)
  (make-face 'company-tooltip-mouse :inherit 'highlight)
  (make-face 'company-tooltip-quick-access :inherit 'company-tooltip-annotation)
  (make-face 'company-tooltip-quick-access-selection :inherit 'company-tooltip-annotation-selection)
  (make-face 'company-tooltip-scrollbar-thumb )
  (make-face 'company-tooltip-scrollbar-track )
  (make-face 'company-tooltip-search :inherit 'highlight)
  (make-face 'company-tooltip-search-selection )
  (make-face 'company-tooltip-selection ))

(defun set-nerdicon-faces (colors)
  (make-face 'nerd-icons-blue :foreground blue)
  (make-face 'nerd-icons-blue-alt :foreground blue-a)
  (make-face 'nerd-icons-cyan :foreground cyan)
  (make-face 'nerd-icons-cyan-alt :foreground cyan-a)
  (make-face 'nerd-icons-dblue :foreground dblue)
  (make-face 'nerd-icons-dcyan :foreground dcyan)
  (make-face 'nerd-icons-dgreen :foreground dgreen)
  (make-face 'nerd-icons-dmaroon :foreground dmaroo)
  (make-face 'nerd-icons-dorange :foreground dorang)
  (make-face 'nerd-icons-dpink :foreground dpink)
  (make-face 'nerd-icons-dpurple :foreground dpurpl)
  (make-face 'nerd-icons-dred :foreground dred)
  (make-face 'nerd-icons-dsilver :foreground dsilve)
  (make-face 'nerd-icons-dyellow :foreground dyello)
  (make-face 'nerd-icons-green :foreground green)
  (make-face 'nerd-icons-lblue :foreground lblue)
  (make-face 'nerd-icons-lcyan :foreground lcyan)
  (make-face 'nerd-icons-lgreen :foreground lgreen)
  (make-face 'nerd-icons-lmaroon :foreground lmaroo)
  (make-face 'nerd-icons-lorange :foreground lorang)
  (make-face 'nerd-icons-lpink :foreground lpink)
  (make-face 'nerd-icons-lpurple :foreground lpurpl)
  (make-face 'nerd-icons-lred :foreground lred)
  (make-face 'nerd-icons-lsilver :foreground lsilve)
  (make-face 'nerd-icons-lyellow :foreground lyello)
  (make-face 'nerd-icons-maroon :foreground maroon)
  (make-face 'nerd-icons-orange :foreground orange)
  (make-face 'nerd-icons-pink :foreground pink)
  (make-face 'nerd-icons-purple :foreground purple)
  (make-face 'nerd-icons-purple-alt :foreground purple)
  (make-face 'nerd-icons-red :foreground red)
  (make-face 'nerd-icons-red-alt :foreground red-a)
  (make-face 'nerd-icons-silver :foreground silver)
  (make-face 'nerd-icons-yellow :foreground yellow))

;; Call The Functions ----------------------------------------------------------------------------------
(custom-theme-set-faces 'template
			(set-default-faces colors))

(provide 'template)
;;; template-theme.el ends here
