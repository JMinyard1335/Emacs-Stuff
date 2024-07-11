(deftheme MagicMan1335 "Magicman1335's custom theme")

(defun make-face (name &rest args)
  (list name `((t, args))))

;;Set up a color pallet for different features
(let (
      ;;Simple Grey-scale
      (black "#000000")
      (grey10 "#1a1a1a")
      (grey20 "#333333")
      (grey30 "#4d4d4d")
      (grey40 "#666666")
      (grey50 "#999999")
      (grey60 "#b3b3b3")
      (grey70 "#b3b3b3")
      (grey80 "#cccccc")
      (grey90 "#e5e5e5")
      (white "#ffffff")

      
      (light_blue "#00ffff")
      (orange "#fc8100")
      (purple "#8a2be2")
      (red_text "#e5786d") ;;Nice red for reading text
      ;;PALETES
      ;;backgrounds
      ;;https://www.color-hex.com/color-palette/104659
      ;;dark->light
      (bg0 "#071104")
      (bg1 "#081c05")
      (bg2 "#0e2408")
      (bg3 "#162d0d")
      (bg4 "#163611")
      (bg5 "#1a3f14")
      ;;gradiant
      ;;green(0)->yellow(9)
      ;;https://colordesigner.io/gradient-generator/?mode=lab#00FF1A-B8CC00
      (color0 "#00ff1a")
      (color1 "#43fa17")
      (color2 "#5df415")
      (color3 "#71ef12")
      (color4 "#81e90f")
      (color5 "#8ee40c")
      (color6 "#9ade08")
      (color7 "#a5d805")
      (color8 "#afd202")
      (color9 "#b8cc00")
      ;;acent colors
      ;;pastel
      ;;https://colordesigner.io/gradient-generator/?mode=lch#7AFF88-F3FF99
      (l_color0 "#7aff88")
      (l_color1 "#8eff87")
      (l_color2 "#9fff86")
      (l_color3 "#aeff86")
      (l_color4 "#bcff87")
      (l_color5 "#c9ff8a")
      (l_color6 "#d5ff8c")
      (l_color7 "#e0ff90")
      (l_color8 "#eaff94")
      (l_color9 "#f3ff99")
      ;;dark
      ;;https://colordesigner.io/gradient-generator/?mode=lch#007B10-6E7A00
      (d_color0 "#007b10")
      (d_color1 "#1f7b0a")
      (d_color2 "#2f7b05")
      (d_color3 "#3c7b01")
      (d_color4 "#467b00")
      (d_color5 "#4f7b00")
      (d_color6 "#587b00")
      (d_color7 "#607b00")
      (d_color8 "#677a00")
      (d_color9 "#6e7a00")

      ;;mostly implamented because of nerdicons
      (blue "#0000ff")  
      (blue-a "#3232ff")
      (cyan "#00FFFF")  
      (cyan-a "#00ffce")
      (dblue "#00008B")
      (dcyan "#009999")
      (dgreen "#006400")
      (dmaroo "#330000")
      (dorang "#FF8C00")
      (dpink "#f62a4e")
      (dpurpl "#330033")
      (dred "#8B0000")
      (dsilve "#393939")
      (dyello "#7f7f00")
      (green "00ff00") 
      (lblue "#ADD8E6")
      (lcyan "#E0FFFF")
      (lgreen "#90EE90")
      (lmaroo "#bf7f7f")
      (lorang "#ffd27f")
      (lpink "#FFB6C1")
      (lpurpl "#bf7fbf")
      (lred "#ff9999")
      (lsilve "#c0c0c0")
      (lyello "#FFFFE0")
      (maroon "#800000")
      (orange "#FFA500")
      (pink "#FFC0CB")  
      (purple "#800080")
      (red "#ff0000")   
      (red-a "#e5786d")
      (silver "#c0c0c0")
      (yellow "FFFF00")
      )
  
  (custom-theme-set-faces 'MagicMan1335
			  ;; BASIC FACES
			  ;;normal emacs windows and features
			  (make-face 'default :background bg0 :foreground grey90 :family "VictorMono Nerd Font")
			  ;;tab-bar and tab-line
			  (make-face 'tab-line :inherit 'default :background bg4 :foreground color9 :height 180 :italic)
			  (make-face 'tab-line-close-highlight :background red :foreground red)
			  (make-face 'tab-line-tab-current :inherit 'default :background bg0 :foreground l_color9 :bold t :italic t)
			  (make-face 'tab-line-tab-inactive :inherit 'default :background bg2 :italic t)
			  (make-face 'tab-bar :inherit 'tab-line)
			  (make-face 'tab-bar-tab :inherit 'tab-line-tab-current)
			  (make-face 'tab-bar-tab-inactive :inherit 'tab-line-tab-inactive)
			  ;;sets up font types
			  (make-face 'bold :inherit 'default :bold t)
			  (make-face 'italic :inherit 'default :italic t)
			  (make-face 'bold-italic :inherit 'default :bold t :italic t)
			  (make-face 'underline :inherit 'default :underline t)
			  (make-face 'shadow )
			  ;;other
			  (make-face 'border)
			  (make-face 'vertical-border :foreground bg1 :background d_color0)
			  (make-face 'fringe :background bg0)
			  (make-face 'region :background l_color9 :foreground bg1)
			  (make-face 'custom-face-tag :bold t :italic t)
			  (make-face 'custom-state green)
			  (make-face 'link :foreground light_blue)
			  (make-face 'link-visited :inherit 'link)
			  (make-face 'cursor :background color0 :foreground black)
			  (make-face 'error :foreground red :underline t)
			  (make-face 'warning :foreground orange :underline t)
			  (make-face 'success :foreground light_blue :underline t)
			  ;;linenumber-mode
			  (make-face 'line-number :inherit 'tab-line :foreground d_color9 :height 130)
			  (make-face 'line-number-current-line :foreground l_color9 :background bg0 :bold t :height 150)
			  ;;others
			  (make-face 'secondary-selection :extend t )
			  
			  ;;FONT LOCK FACES
			  ;;used for syntax highlighting
			  (make-face 'font-lock-warning-face :foreground red)
			  (make-face 'font-lock-function-name-face :foreground color0 :italic t)
			  (make-face 'font-lock-variable-name-face :foreground color2)
			  (make-face 'font-lock-variable-use-face :inheqrit 'font-lock-variable-name-face)
			  (make-face 'font-lock-keyword-face :foreground l_color9)
			  (make-face 'font-lock-comment-face :foreground grey60)
			  (make-face 'font-lock-comment-delimiter-face :inherit 'font-lock-comment-face)
			  (make-face 'font-lock-type-face :foreground l_color1)
			  (make-face 'font-lock-constant-face :foreground l_color0 :bold t)
			  (make-face 'font-lock-builtin-face :foreground color4)
			  (make-face 'font-lock-preprocessor-face :foreground d_color9 :italic t)
			  (make-face 'font-lock-string-face :foreground color6)
			  (make-face 'font-lock-doc-face  )
			  (make-face 'font-lock-doc-markup-face )
			  (make-face 'font-lock-negation-char-face :foreground red_text)
			  (make-face 'font-lock-escape-face :foreground color9)

			  ;;Modeline/Doom-Modeline
			  (make-face 'mode-line :foreground color9 :background bg4 :italic t)
			  (make-face 'mode-line-inactive :background bg0)
			  (make-face 'mode-line-highlight :background l_color9 :foreground black :bold t)
			  (make-face 'mode-line-buffer-id :foreground color3 :background bg0)
			  ;;Basic Faces for doom-mode line most others inherit from this set
			  (make-face 'doom-modeline :inherit 'nerd-icons-faces :background bg4 :italic t) ;; dosent seem to be the whole bar
			  (make-face 'doom-modeline-bar :inherit 'mode-line-highlight) ;; little sliver on the left side of the bar
			  (make-face 'doom-modeline-bar-inactive :inherit 'mode-line-inactive)
			  (make-face 'doom-modeline-emphasis :bold t)
			  (make-face 'doom-modeline-warning :foreground orange)
			  (make-face 'doom-modeline-urgent :foreground red)
			  (make-face 'doom-modeline-info :foreground light_blue)
			  (make-face 'doom-modeline-highlight :inherit 'mode-line-highlight)
			  (make-face 'doom-modeline-time :foreground grey60)
			  ;;Battery 
			  (make-face 'doom-modeline-battery-normal :inherit 'doom-modeline)
			  (make-face 'doom-modeline-battery-charging :foreground color0)
			  (make-face 'doom-modeline-battery-critical :inherit 'doom-modeline-urgent)
			  (make-face 'doom-modeline-battery-error :inherit 'doom-modeline-urgent)
			  (make-face 'doom-modeline-battery-full :inherit 'doom-modeline-info)
			  (make-face 'doom-modeline-battery-warning :inherit 'doom-modeline-warning)
			  ;;Boon
			  (make-face 'doom-modeline-boon-command-state :inherit 'doom-modeline-info)
			  (make-face 'doom-modeline-boon-insert-state :inherit '(doom-modeline font-lock-keyword-face))
			  (make-face 'doom-modeline-boon-off-state :inherit '(mode-line doom-modeline))
			  (make-face 'doom-modeline-boon-special-state :inherit '(doom-modeline font-lock-builtin-face))
			  ;;Buffer info
			  (make-face 'doom-modeline-buffer-file :inherit '(doom-modeline mode-line-buffer-id) :bold t)
			  (make-face 'doom-modeline-buffer-major-mode :inherit 'doom-modeline-emphasis :bold t)
			  (make-face 'doom-modeline-minor-modes :inherit 'font-lock-doc-face)
			  (make-face 'doom-modeline-buffer-modified :inherit 'doom-modeline-warning :bold t)
			  (make-face 'doom-modeline-buffer-path :inherit 'doom-modeline-emphasis :bold t)
			  (make-face 'doom-modeline-buffer-timemachine :foreground red)
			  ;;complilation
			  (make-face 'doom-modeline-compilation :height 0.9 :inherit 'doom-modeline-warning)
			  ;;debug
			  (make-face 'doom-modeline-debug :inherit '(doom-modeline font-lock-doc-face))
			  (make-face 'doom-modeline-debug-visual :inherit 'doom-modeline)
			  ;;evil "i dont use this"
			  (make-face 'doom-modeline-evil-emacs-state :inherit '(doom-modeline font-lock-builtin-face))
			  (make-face 'doom-modeline-evil-insert-state :inherit '(doom-modeline font-lock-keyword-face))
			  (make-face 'doom-modeline-evil-motion-state :inherit '(doom-modeline font-lock-doc-face))
			  (make-face 'doom-modeline-evil-normal-state :inherit 'doom-modeline-info)
			  (make-face 'doom-modeline-evil-operator-state :inherit '(doom-mode-line doom-modeline-info))
			  (make-face 'doom-modeline-evil-replace-state :inherit 'doom-modeline-urgent)
			  (make-face 'doom-modeline-evil-visual-state :inherit 'doom-modeline-warning)
			  ;;fly
			  (make-face 'doom-modeline-fly-insert-state :inherit '(doom-modeline font-lock-keyword-face))
			  (make-face 'doom-modeline-fly-normal-state :inherit 'doom-modeline-info)
			  ;;input
			  (make-face 'doom-modeline-input-method :inherit 'doom-modeline-emphasis)
			  (make-face 'doom-modeline-input-method-alt :inherit '(doom-modelinie font-lock-doc-face))
			  ;;lap
			  (make-face 'doom-modeline-lsp-error :inherit 'doom-modeline-urgent)
			  (make-face 'doom-modeline-lsp-running :inherit '(doom-mode-line compilation-mode-line-run))
			  (make-face 'doom-modeline-lsp-success :inherit 'doom-modeline-info)
			  (make-face 'doom-modeline-lsp-warning :inherit 'doom-modeline-warning)
			  ;;project
			  (make-face 'doom-modeline-project-dir :inherit 'mode-line)
			  (make-face 'doom-modeline-project-parent-dir :inherit 'doom-modeline-project-dir)
			  (make-face 'doom-modeline-project-root-dir 'doom-modeline-project-dir)
			  ;;repel
			  (make-face 'doom-modeline-repl-success :inherit 'doom-modeline-info)
			  (make-face 'doom-modeline-repl-warning :inherit 'doom-modeline-warning)
			  ;;other
			  (make-face 'doom-modeline-host :italic t) ;; add a forground of choice
			  (make-face 'doom-modeline-god :inherit 'doom-modeline-info)
			  (make-face 'doom-modeline-notification :inherit 'doom-modeline-warning)
			  (make-face 'doom-modeline-overwrite :inherit 'doom-modeline-urgent)
			  (make-face 'doom-modeline-panel :inherit 'doom-modeline-highlight)
			  (make-face 'doom-modeline-ryo :inherit 'doom-modeline-info)
			  (make-face 'doom-modeline-unread-number :inherit 'doom-modeline)
			  (make-face 'doom-modeline--icon :foreground red)
			  
			  ;;ORGMODE
			  ;;agenda
			  (make-face 'org-agenda-structure :foreground "yellow" :italic t :height 180 :underline t)
			  (make-face 'org-agenda-calendar-event :italic t)
			  (make-face 'org-agenda-calendar-sexp :italic t)
			  (make-face 'org-agenda-clocking :inherit 'secondary-selection)
			  (make-face 'org-agenda-column-dateline :inherit 'org-column)
			  (make-face 'org-agenda-current-time :inherit 'org-time-grid :italic t)
			  (make-face 'org-agenda-date :foreground d_color3 :italic t :height 150)
			  (make-face 'org-agenda-date-today :foreground color1 :italic t :height 165)
			  (make-face 'org-agenda-date-weekend :inherit 'org-agenda-date :foreground d_color9)
			  (make-face 'org-agenda-diary :inherit 'default)
			  (make-face 'org-agenda-dimmed-todo-face :foreground purple :italic t)
			  (make-face 'org-agenda-done :foreground l_color0 :italic t)
			  (make-face 'org-agenda-filter-category :inherit 'mode-line)
			  (make-face 'org-agenda-filter-effort :inherit 'mode-line)
			  (make-face 'org-agenda-filter-regexp :inherit 'mode-line)
			  (make-face 'org-agenda-filter-tags :inherit 'mode-line)
			  (make-face 'org-agenda-restriction-lock :background l_color9)
			  ;; code block
			  (make-face 'org-block :extend t :background bg2)
			  (make-face 'org-block-begin-line :extend t :background bg1 :foreground grey80)
			  (make-face 'org-block-end-line :inherit 'org-block-begin-line)
			  ;;checkboxes
			  (make-face 'org-checkbox )
			  (make-face 'org-checkbox-statistics-done :inherit 'org-done)
			  (make-face 'org-checkbox-statistics-todo :inherit 'org-todo)
			  ;;clock/dates/times
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
			  (make-face 'org-time-grid :italic t)
			  ;;documents
			  (make-face 'org-document-info-keyword :inherit 'shadow)
			  (make-face 'org-document-info :inherit 'org-document-info-keyword)
			  (make-face 'org-document-title :inherit 'org-document-info-keyword)
			  ;; Org Header
			  (make-face 'org-level-1 :height 1.50 :foreground color1)
			  (make-face 'org-level-2 :foreground color2 :height 1.35)
			  (make-face 'org-level-3 :foreground color3 :height 1.30)
			  (make-face 'org-level-4 :foreground color4 :height 1.25)
			  (make-face 'org-level-5 :foreground color5 :height 1.20)
			  (make-face 'org-level-6 :foreground color6 :height 1.15)
			  (make-face 'org-level-7 :foreground color7 :height 1.10)
			  (make-face 'org-level-8 :foreground color8 :height 1.05)
			  ;;habit
			  (make-face 'org-habit-alert-face :background)
			  (make-face 'org-habit-alert-future-face :background)
			  (make-face 'org-habit-clear-face :background)
			  (make-face 'org-habit-clear-future-face :background)
			  (make-face 'org-habit-overdue-face :background red)
			  (make-face 'org-habit-overdue-future-face :background red_text)
			  (make-face 'org-habit-ready-face :background)
			  (make-face 'org-habit-ready-future-face :background)
			  ;;tags
			  (make-face 'org-tag :foreground d_color9 :italic t)
			  (make-face 'org-tag-faces :inherit 'org-tags)
			  (make-face 'org-tag-group :inherit 'org-tags)
			  ;;states
			  (make-face 'org-todo :foreground red_text :bold t :height 1.3)
			  (make-face 'org-todo-keyword-faces :inherit 'org-todo)
			  (make-face 'org-done :foreground d_color0 :bold t :height 1.3)
			  (make-face 'org-headline-done :inherit 'org-done)
			  ;;other
			  (make-face 'org-default :inherit 'default)
			  (make-face 'org-archived :inherit 'shadow)
			  (make-face 'org-code :inherit 'shadow)
			  (make-face 'org-column :foreground purple)
			  (make-face 'org-column-title :inherit 'org-column)
			  (make-face 'org-drawer :foreground l_color8)
			  (make-face 'org-ellipsis :foreground purple)
			  (make-face 'org-footnote :foreground purple)
			  (make-face 'org-formula :foreground purple)
			  (make-face 'org-hide :foreground bg0) ;;hides the leading '*' in headers and someother things
			  (make-face 'org-indent :inherit 'org-hide)
			  (make-face 'org-macro :inherit 'org-latex-and-related)
			  (make-face 'org-link :inherit 'link)
			  (make-face 'org-list-dt :bold t)
			  (make-face 'org-meta-line :inherit 'font-lock-comment-face)
			  (make-face 'org-priority :inherit 'font-lock-keyword-face)
			  (make-face 'org-quote :foreground purple)
			  (make-face 'org-warning :foreground orange :bold t :italic t)
			  (make-face 'org-latex-and-related :foreground purple)
			  (make-face 'org-target :underline t)
			  
			  ;;RAINBOW-DELIMITERS
			  (make-face 'rainbow-delimiters-depth-1-face :foreground color0)
			  (make-face 'rainbow-delimiters-depth-2-face :foreground color1)
			  (make-face 'rainbow-delimiters-depth-3-face :foreground color2)
			  (make-face 'rainbow-delimiters-depth-4-face :foreground color3)
			  (make-face 'rainbow-delimiters-depth-5-face :foreground color4)
			  (make-face 'rainbow-delimiters-depth-6-face :foreground color5)
			  (make-face 'rainbow-delimiters-depth-7-face :foreground color6)
			  (make-face 'rainbow-delimiters-depth-8-face :foreground color7)
			  (make-face 'rainbow-delimiters-depth-9-face :foreground color8)
			  (make-face 'rainbow-delimiters-mismatched-face :foreground red)
			  (make-face 'rainbow-delimiters-unmatched-face :foreground orange)

			  ;;COMPANY_MODE
			  (make-face 'company-echo :inherit 'default)
			  (make-face 'company-echo-common :inherit 'company-echo)
			  (make-face 'company-preview :inherit '(company-tooltip-selection company-tooltip))
			  (make-face 'company-preview-common :inherit 'company-tooltip-common-selection)
			  (make-face 'company-preview-search :inherit 'company-tooltip-common-selection)
			  (make-face 'company-template-field :foreground color9)
			  (make-face 'company-tooltip :background bg4 :foreground grey80 :extend t)
			  (make-face 'company-tooltip-annotation :foreground color9)
			  (make-face 'company-tooltip-annotation-selection :foreground black)
			  (make-face 'company-tooltip-common :foreground color0)
			  (make-face 'company-tooltip-common-selection :foreground d_color0)
			  (make-face 'company-tooltip-deprecated :strike-through t :foreground red)
			  (make-face 'company-tooltip-mouse :inherit 'highlight)
                          (make-face 'company-tooltip-quick-access :inherit 'company-tooltip-annotation)
			  (make-face 'company-tooltip-quick-access-selection :inherit 'company-tooltip-annotation-selection)
			  (make-face 'company-tooltip-scrollbar-thumb :background d_color0)
			  (make-face 'company-tooltip-scrollbar-track :background d_color9)
			  (make-face 'company-tooltip-search :inherit 'highlight)
			  (make-face 'company-tooltip-search-selection :inherit 'highlight)
			  (make-face 'company-tooltip-selection :foreground bg4 :background l_color9)

			  ;; nerd-icons
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
			  (make-face 'nerd-icons-yellow :foreground yellow)

			  ;;lsp-ui
			  (make-face 'lsp-ui-sideline-global :inherit 'variable-pitch)
			  ))


(provide 'MagicMan1335)
