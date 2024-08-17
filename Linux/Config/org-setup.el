;;; org-setup.el --- Contains Configuration for org-mode  -*- lexical-binding: t; -*-

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

;; This holds configurations for org-mode and org-agenda. Basically if its org it can be found in this file.
;; This includes things like:
;; 1. Org-mode
;; 2. Org-Agenda
;; 3. Org-Babel
;; 4. Org-Super-Agenda
;; 5. Org-Modern

;;; Code:
(use-package org-capture)

(defgroup my-org nil
  "Customizations for org-mode."
  :group nil)

(defconst my-time_regex
  "\\([01][0-9]\\|2[0-3]\\):[0-5][0-9]\\(:[0-5][0-9]\\)?"
  "This is a regex string to allow HH:MM:SS time formats
It is in the 24hr time format and will accept the formats
HH:MM:SS or HH:MM")

(defun my-helper-validate-time (string)
  "Validates the time string in the form of HH:MM:SS"
  (if (string-match-p my-time_regex string)
      t
    nil))

(defun my-helper-replace-time-fields (string)
  "this function takes in a time string that that is in the format of <yyyy-mm-dd HH:MM +ni -ni>
If any of these fields are empty this function removes the spaces from the string ie: <yyyy-mm-dd     >
will become <yyyy-mm-dd>."
  (replace-regexp-in-string "[ \t>]*$" ">" string))

(defun my-agenda-set-deadline ()
  "This function creates a deadline timestamp.
This function will prompt a date, time, repeat interval, and a warning period."
  (my-helper-replace-time-fields
   (format "<%s%s%s%s>"
	   (my-agenda-get-date)
	   (my-agenda-get-time)
	   (my-agenda-get-repeat)
	   (my-agenda-get-warning))))

(defun my-agenda-set-class-time ()
  "This function is used to set the time for a class."
  (my-helper-replace-time-fields
   (format "<%s%s%s>"
	   (my-agenda-get-date)
	   (my-agenda-get-time)
	   (my-agenda-get-repeat))))

(defun my-agenda-get-date ()
  "This function is to be used as part of a function to create a date format string.
It prompts the user to enter a date using the built-in `org-read-date'."
  (let ((date (org-read-date nil t nil "Enter the date ")))
    (message (format-time-string "%Y-%m-%d" date))))

(defun my-agenda-get-time ()
  "This function is used to get a time from the user."
  (if (y-or-n-p "Would you like to set a time? ")
      (let ((time (read-string "Enter the time: ")))
	(if (not (my-helper-validate-time time))
	    (setq time (read-string "Use the format HH:MM:SS or HH:MM"))
	  (format " %s" time)))
    (format "" )))

(defun my-agenda-get-repeat ()
  "Ask the user if they want the time stamp to include a repeat value.
This value is of the for +nc where n is an integer and c is a choice between
'(Year Month Week Day Hour). The user must pick the interval type and the number"
  (if (y-or-n-p "Would you like to set a repeat interval? ")
      (let ((interval (my-agenda-repeat-interval))
	    (number (read-number "Enter the number of intervals: ")))
	(format " +%d%s" number interval))
    (format " +0d")))

(defun my-agenda-repeat-interval ()
  "This function is used to get and return the char needed to set a repeat interval.
This function allows the user to pick between several options '(Year Month Day Hour).
Depending on the value picked by the user a character is returned '(y m d h)."
  (let ((choice (completing-read "Select repeat interval: " '("Year" "Month" "Day" "Hour" "Week") nil t)))
    (cond ((string= choice "Year") "y")
	  ((string= choice "Month") "m")
	  ((string= choice "Day") "d")
	  ((string= choice "Hour") "h")
	  ((string= choice "Week") "w"))))

(defun my-agenda-get-warning ()
  "Ask the user if they want the time stamp to include a warning value.
This value is of the for -nc where n is an integer and c is a choice between
'(Year Month Week Day Hour). The user must pick the interval type and the number"
  (if (y-or-n-p "Would you like to set a warning? ")
      (let ((interval (my-agenda-repeat-interval))
	    (number (read-number "Enter the number of intervals: ")))
	(format " -%d%s" number interval))
    (format "")))

(defun my-agenda-link-file ()
  "Prompts the user if they would like to insert a link to a file.
This link is of the type `file' and is used to link files to tasks.
If the user selects yes, they will be prompted to select a file to link.
If the user selects no, `nil' is inserted in its place."
  (if (y-or-n-p "Would you like to link a file to this task? ")
      (format "[[file:%s]]" (read-file-name "File to link: "))
    (format "NA" )))

(defun my-org-visual-setup ()
  "This function is used to set the visual look of org-mode.
This function centers the text on the screen and enables line wrapping.
It also makes sure any faces that need to be fixed width are set
to fixed width."
  (setq org-adapt-indentation t
	visual-fill-column-width 150
	visual-fill-column-center-text t)
  (visual-line-fill-column-mode 1))

(defun my-org-faces ()
  "makes any faces that need to be fixed width fixed width."
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch))

(defun my-org-agenda-files (files)
  "Takes in a list of strings as file names and adds them to `org-agenda'"
  (dolist (file files)
    (add-to-list 'org-agenda-files file)))

(defun my-org-logging ()
  "This funtion controls how logging is done in org-mode."
  (setq org-log-into-drawer "Logbook"
	org-clock-into-drawer "Logbook"
	org-log-done 'time
	org-log-repeat 'time
	org-log-reschedule 'time
	org-log-redeadline 'time))

(defun my-org-todo ()
  "This function defines some additional todo keywords.
It also makes sure that all sub task are completed before a parent."
  (setq org-enforce-todo-checkbox-dependencies t
	org-enforce-todo-dependencies t
	org-closed-keep-when-no-todo t)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-todo-keywords-for-agenda '("TODO(t)" "WAITING(w)"))
  (setq org-done-keywords-for-agenda '("DONE(d)" "CANCELED(c)")))

(defun my-org-tags ()
  "This function sets up tags and how they are displayed."
  (setq org-auto-align-tags t
	org-tags-column -80
	org-agenda-tags-column 0
	org-fast-tag-selection-single-key nil
	org-use-tag-inheritance t)
  (setq org-tag-alist '(("school" . ?s)
			("work" . ?w)
			("project" . ?p)))
  (setq org-tags-exclude-from-inheritance '()))

(defun my-org-agenda ()
  "This function is in charge of the org-agenda settings."
  (setq org-agenda-window-setup 'current-window))

(use-package doct
  :ensure t
  :commands (doct))

(defun my-school-capture ()
  "This function holds the template for the school capture."
  '("School" :keys "s"
    :file (lambda () (concat org-directory "school.org"))
    :template
    ("* %{todo-state} %^{task} %{tags}"
     "%{time-stamp}"
     ":Logbook:"
     ":Class: %^{class}"
     ":Assigned: %U"
     ":link: %(my-agenda-link-file)"
     ":END:")
    :children (("Assignment" :keys "a"
		:todo-state "TODO"
		:tags ":assignment:"
		:time-stamp ":DEADLINE: %(my-agenda-set-deadline)")
	       ("Labs" :keys "l"
		:todo-state "TODO"
		:tags ":lab:"
		:time-stamp ":SCHEDULED: %(my-agenda-set-deadline)")
	       ("Exam" :keys "e"
		:todo-state "TODO"
		:tags ":exam:"
		:time-stamp ":SCHEDULED: %(my-agenda-set-deadline)"))))

(defun my-school-agenda ()
  "This function defines an agenda view for school tasks."
  '("s" "School Agenda"
    ((agenda
      ""
      ((org-agenda-span 'day)
       (org-todo-keywords
	'((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
       (org-todo-keywords-for-agenda '("TODO(t)" "WAITING(w)"))
       (org-done-keywords-for-agenda '("DONE(d)" "CANCELED(c)"))
       (org-agenda-time-grid nil)
       (org-agenda-overriding-header "School Agenda")
       (org-agenda-prefix-format '((agenda . "  %i %s %t ")))
       (org-agenda-hide-tags-regexp ".*")
       (org-super-agenda-unmatched-name "Misc")
       (org-super-agenda-groups
	'((:name "Overdue"
		 :deadline past
		 :order 0)
	  (:name "Today"
		 :deadline today
		 :order 1)
	  (:name "Assignments"
		 :tag "assignment"
		 :order 2)
	  (:name "Labs"
		 :tag "lab"
		 :order 2)
	  (:name "Projects"
		 :tag "project"
		 :order 2)
	  (:name "Exams"
		 :tag "exam"
		 :order 2))))))))

(use-package org-modern
  :ensure t)

(use-package org-super-agenda
  :ensure t
  :hook
  (org-agenda-mode . org-super-agenda-mode)
  :config
  (setq org-super-agenda-hide-empty-groups nil))

(use-package org
  :init
  (setq inhibit-compacting-font-caches t)
  :hook
  (org-mode . my-org-visual-setup)
  :config
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(use-package olivetti 
  :ensure t
  :hook
  (org-agenda-mode . olivetti-mode)
  :config
  (setq olivetti-style 'fancy))

;; ;; The main org-mode package.
;; (use-package org
;;   :init (setq inhibit-compacting-font-caches t)
;;   :hook (org-mode . my/org-mode-setup)
;;   :config
;;   (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
;;   (setq org-agenda-window-setup 'current-window)
;;   (setq org-log-into-drawer t)
;;   (setq org-log-done '('time))
;;   (setq org-todo-keywords
;; 	'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")
;; 	  (sequence "ASSIGNED(A)" "CURRENT(C)" "|" "TURNEDIN(T)")))
;;   (setq org-todo-keywords-for-agenda '("TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "ASSIGNED(A)" "CURRENT(C)"))
;;   (setq org-done-keywords-for-agenda '("DONE(d)" "CANCELED(c)" "TURNEDIN(T)"))
;;   (setq org-deadline-warning-days 7)
;;   (setq org-agenda-hide-tags-regexp ".*")
;;   (setq org-adapt-indentation t)
;;   (setq org-agenda-tags-column 40)
;;   (setq org-hide-drawer-startup t)
;;   (my/org-font-setup))

;; ;; Allows for better grouping in org-agenda
;; (use-package org-super-agenda 
;;   :ensure t
;;   :hook (org-agenda-mode . org-super-agenda-mode))

;; ;; Changes the look of the org-agenda.
;; (use-package org-modern 
;;   :ensure t)

;; (use-package org-bullets
;;   :ensure t
;;   :hook (org-mode . org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; ;; Used to center the text in org-mode.
;; ;; I only use this for the agenda.


;; ;; Delcartive Org Capture Templates.
;; ;; creates a new interface for creating capture templates.
;; (use-package doct 
;;   :ensure t
;;   :commands (doct))

;; (use-package visual-fill-column
;;   :ensure t
;;   :hook (org-mode . my/org-mode-visual-fill))

;; ;; Set up the Capture Templates ----------------------------------------------------------------------------
;; ;; These capture templates can be added to the template list in `org-capture-templates'
;; ;; Or they can be called by hotkeys bounded to (let) calls that call the `org-capture' function
;; (defun idemacs/capture-school-template ()
;;   "A list to hold the capture templates for school tasks."
;;   '("School Work" :keys "s"
;;     :file idemacs-school-path
;;     :template
;;     ("* %{todo-state} %{task} %{tags}:school:%^G"
;;      "%{time-stamp}"
;;      ":PROPERTIES:"   
;;      ":Class: %^{class}"
;;      ":Assigned: %U"
;;      "%{link}"
;;      ":END:")
;;     :children
;;     (("Assignment" :keys "a"
;;      :headline "Assignments"
;;      :todo-state "ASSIGNED"
;;      :task "%^{Assignment}"
;;      :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
;;      :link ":Assignment: %(idemacs/agenda-link-file)"
;;      :tags ":homework")
;;      ("Lab" :keys "l"
;;       :headline "Labs"
;;       :todo-state "ASSIGNED"
;;       :task "%^{Lab}"
;;       :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
;;       :link ":Lab: %(idemacs/agenda-link-file)"
;;       :tags ":lab")
;;      ("Exams" :keys "e"
;;       :headline "Exams"
;;       :todo-state ""
;;       :task "%^{Exam}"
;;       :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
;;       :link ":Review: %(idemacs/agenda-link-file)"
;;       :tags ":exam")
;;      ("Class" :keys "c"
;;       :headline "Classes"
;;       :template
;;       ("* TODO %^{Class} :school:class:"
;;        "%(idemacs/agenda-set-class-time) %(idemacs/agenda-set-class-time)"
;;        ":PROPERTIES:"
;;        ":Course_Number: %^{Course Number}"
;;        ":Section_Number: %^{Section Number}"
;;        ":Instructor: %^{Instructor}"
;;        ":Location: %^{Location}"
;;        ":Class_Directory: %(idemacs/agenda-link-file)"
;;        ":END:")))))

;; (defun idemacs/capture-project-template ()
;;  "A list to hold the capture templates for projects."
;;   '("Projects" :keys "p"
;;     :template
;;     ("* %{todo-state} %^{Task} %{tags}:project:%^G"
;;      "%{time-stamp}"
;;      ":PROPERTIES:"
;;      ":Description: %?"
;;      "%{created}"
;;      "%{link}"
;;      "%{project}"
;;      ":END:")
;;     :children
;;     (("School Projects" :keys "s"
;;       :file idemacs-school-path
;;       :todo-state "ASSIGNED"
;;       :tags ":school"
;;       :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
;;       :created ":Assigned: %U"
;;       :children
;;       (("Project" :keys "p"
;; 	:headline "Projects"
;; 	:link ":Project: %(idemacs/agenda-link-file)"
;; 	:project "%^{What is the name of the project?}")
;;        ("Sub Task" :keys "t"
;; 	:function idemacs/agenda-school-project-capture
;; 	:link ":Task: %(idemacs/agenda-link-file)"
;; 	:project "%^{What is the name of the project? }")))
;;      ("Personal Projects" :keys "p"
;;       :file idemacs-personal-path
;;       :todo-state "TODO"
;;       :tags ":personal"
;;       :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
;;       :created ":Created: %U"
;;       :children
;;       (("Project" :keys "p"
;; 	:headline "Projects"
;; 	:link ":Personal_Docs: %(idemacs/agenda-link-file)"
;; 	:project ":Project: %^{What is the name of the project?}")
;;        ("Sub Task" :keys "t"
;; 	:function idemacs/agenda-personal-project-capture
;; 	:link ":Task: %(idemacs/agenda-link-file)"
;; 	:project ":Project: %^{What is the name of the project? }")))
;;      ("Home Projects" :keys "h"
;;       :file idemacs-home-path
;;       :todo-state "TODO"
;;       :tags ":home"
;;       :time-stamp "%(idemacs/agenda-set-deadline)"
;;       :created ":Created: %U"
;;       :children
;;       (("Project" :keys "p"
;; 	:headline "Projects"
;; 	:link ":Home_Docs: %(idemacs/agenda-link-file)"
;; 	:project ":Project: %^{What is the name of the project?}")
;;        ("Sub Task" :keys "t"
;; 	:function idemacs/agenda-home-project-capture
;; 	:link ":Task: %(idemacs/agenda-link-file"
;; 	:project ":Project: %^{What is the name of the project? "))))))

;; (defun idemacs/capture-home-template ()
;;  "A list to hold the capture templates for home tasks."
;;   '("Home" :keys "h"
;;     :file idemacs-home-path
;;     :template
;;     ("* TODO %^{Task} %{tags}%^g"
;;      "SCHEDULED: %(idemacs/agenda-set-deadline)"
;;      ":PROPERTIES:"
;;      ":DATE: %U"
;;      ":END:")
;;     :children
;;     (("Chores" :keys "c"
;;       :headline "Chores"
;;       :tags ":home:errand:"
;;       )
;;      ("Errands" :keys "e"
;;       :headline "Errands"
;;       :tags ":home:errand:"))))

;; (defun idemacs/capture-personal-template ()
;;   "A list to hold the capture templates for personal tasks."
;;   '("Personal" :keys "i"
;;     :file idemacs-personal-path
;;     :children
;;     (("Reminders" :keys "r"
;;       :headline "Reminders"
;;       :template
;;       ("* TODO %^{Reminder} :personal:reminders:%^g"
;;        "DEADLINE: %(idemacs/agenda-set-deadline)"))
;;      ("Goals" :keys "g"
;;       :headline "Goals"
;;       :template
;;       ("* %^{Goal} :personal:goals:%^g"
;;        "%?"
;;        ":PROPERTIES:"
;;        ":DATE: %U"
;;        ":REASON: %^{Why is this a goal?}"
;;        ":END:"))
;;      ("Family" :keys "f"
;;       :headline "Family"
;;       :template
;;       ("* TODO %^{Activity} :personal:family:%^g"
;;        ":PROPERTIES:"
;;        ":DATE: %(idemacs/agenda-set-deadline)"
;;        ":LOCATION: %^{Location}"
;;        ":END:")))))

(provide 'org-setup)
;;; org-setup.el ends here
