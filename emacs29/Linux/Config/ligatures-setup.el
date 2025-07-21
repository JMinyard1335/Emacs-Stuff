;;; ligatures-setup.el --- Sets up Ligatures for certain modes  -*- lexical-binding: t; -*-

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

;; Sets up Ligatures for certain modes

;;; Code:

(use-package ligature
  :ensure t
  :hook (prog-mode . global-ligature-mode)
  :config
  (ligature-set-ligatures '(org-mode)
			  '("->" "<-" "-->" "<--" "<-->"
                            "|>" "<|" "=>" "==>" "::")
                          ;; '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                          ;;   ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                          ;;   "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                          ;;   "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                          ;;   "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                          ;;   "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                          ;;   "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                          ;;   "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                          ;;   ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                          ;;   "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                          ;;   "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                          ;;   "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                          ;;   "\\\\" "://")
			  )) 

(global-ligature-mode t)

(provide 'ligatures-setup)
;;; ligatures-setup.el ends here
