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
(defun my/org-mode-setup ()
  (setq org-adapt-indentation t)
  (visual-line-mode 1))

(defun my/org-mode-visual-fill ()
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun my/org-font-setup ()
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun my/org-agenda-files (files)
  "Takes in a list of strings as file names and adds them to `org-agenda'"
  (dolist (file files)
    add-to-list 'org-agenda-files file))
;; Sets up the packages -------------------------------------------------------------------------------------
(use-package org-capture)

;; The main org-mode package.
(use-package org
  :init (setq inhibit-compacting-font-caches t)
  :hook (org-mode . my/org-mode-setup)
  :config
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (setq org-agenda-window-setup 'current-window)
  (setq org-log-into-drawer t)
  (setq org-log-done '('time))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")
	  (sequence "ASSIGNED(A)" "CURRENT(C)" "|" "TURNEDIN(T)")))
  (setq org-todo-keywords-for-agenda '("TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "ASSIGNED(A)" "CURRENT(C)"))
  (setq org-done-keywords-for-agenda '("DONE(d)" "CANCELED(c)" "TURNEDIN(T)"))
  (setq org-deadline-warning-days 7)
  (setq org-agenda-hide-tags-regexp ".*")
  (setq org-adapt-indentation t)
  (setq org-agenda-tags-column 40)
  (setq org-hide-drawer-startup t)
  (my/org-font-setup))

;; Allows for better grouping in org-agenda
(use-package org-super-agenda 
  :ensure t
  :hook (org-agenda-mode . org-super-agenda-mode))

;; Changes the look of the org-agenda.
(use-package org-modern 
  :ensure t)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Used to center the text in org-mode.
;; I only use this for the agenda.
(use-package olivetti 
  :ensure t
  :hook
  (org-agenda-mode . olivetti-mode))

;; Delcartive Org Capture Templates.
;; creates a new interface for creating capture templates.
(use-package doct 
  :ensure t
  :commands (doct))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . my/org-mode-visual-fill))
;; Set up the Capture Templates ----------------------------------------------------------------------------
;; These capture templates can be added to the template list in `org-capture-templates'
;; Or they can be called by hotkeys bounded to (let) calls that call the `org-capture' function
(defun idemacs/capture-school-template ()
  "A list to hold the capture templates for school tasks."
  '("School Work" :keys "s"
    :file idemacs-school-path
    :template
    ("* %{todo-state} %{task} %{tags}:school:%^G"
     "%{time-stamp}"
     ":PROPERTIES:"   
     ":Class: %^{class}"
     ":Assigned: %U"
     "%{link}"
     ":END:")
    :children
    (("Assignment" :keys "a"
     :headline "Assignments"
     :todo-state "ASSIGNED"
     :task "%^{Assignment}"
     :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
     :link ":Assignment: %(idemacs/agenda-link-file)"
     :tags ":homework")
     ("Lab" :keys "l"
      :headline "Labs"
      :todo-state "ASSIGNED"
      :task "%^{Lab}"
      :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
      :link ":Lab: %(idemacs/agenda-link-file)"
      :tags ":lab")
     ("Exams" :keys "e"
      :headline "Exams"
      :todo-state ""
      :task "%^{Exam}"
      :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
      :link ":Review: %(idemacs/agenda-link-file)"
      :tags ":exam")
     ("Class" :keys "c"
      :headline "Classes"
      :template
      ("* TODO %^{Class} :school:class:"
       "%(idemacs/agenda-set-class-time) %(idemacs/agenda-set-class-time)"
       ":PROPERTIES:"
       ":Course_Number: %^{Course Number}"
       ":Section_Number: %^{Section Number}"
       ":Instructor: %^{Instructor}"
       ":Location: %^{Location}"
       ":Class_Directory: %(idemacs/agenda-link-file)"
       ":END:")))))

(defun idemacs/capture-project-template ()
 "A list to hold the capture templates for projects."
  '("Projects" :keys "p"
    :template
    ("* %{todo-state} %^{Task} %{tags}:project:%^G"
     "%{time-stamp}"
     ":PROPERTIES:"
     ":Description: %?"
     "%{created}"
     "%{link}"
     "%{project}"
     ":END:")
    :children
    (("School Projects" :keys "s"
      :file idemacs-school-path
      :todo-state "ASSIGNED"
      :tags ":school"
      :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
      :created ":Assigned: %U"
      :children
      (("Project" :keys "p"
	:headline "Projects"
	:link ":Project: %(idemacs/agenda-link-file)"
	:project "%^{What is the name of the project?}")
       ("Sub Task" :keys "t"
	:function idemacs/agenda-school-project-capture
	:link ":Task: %(idemacs/agenda-link-file)"
	:project "%^{What is the name of the project? }")))
     ("Personal Projects" :keys "p"
      :file idemacs-personal-path
      :todo-state "TODO"
      :tags ":personal"
      :time-stamp "DEADLINE: %(idemacs/agenda-set-deadline)"
      :created ":Created: %U"
      :children
      (("Project" :keys "p"
	:headline "Projects"
	:link ":Personal_Docs: %(idemacs/agenda-link-file)"
	:project ":Project: %^{What is the name of the project?}")
       ("Sub Task" :keys "t"
	:function idemacs/agenda-personal-project-capture
	:link ":Task: %(idemacs/agenda-link-file)"
	:project ":Project: %^{What is the name of the project? }")))
     ("Home Projects" :keys "h"
      :file idemacs-home-path
      :todo-state "TODO"
      :tags ":home"
      :time-stamp "%(idemacs/agenda-set-deadline)"
      :created ":Created: %U"
      :children
      (("Project" :keys "p"
	:headline "Projects"
	:link ":Home_Docs: %(idemacs/agenda-link-file)"
	:project ":Project: %^{What is the name of the project?}")
       ("Sub Task" :keys "t"
	:function idemacs/agenda-home-project-capture
	:link ":Task: %(idemacs/agenda-link-file"
	:project ":Project: %^{What is the name of the project? "))))))

(defun idemacs/capture-home-template ()
 "A list to hold the capture templates for home tasks."
  '("Home" :keys "h"
    :file idemacs-home-path
    :template
    ("* TODO %^{Task} %{tags}%^g"
     "SCHEDULED: %(idemacs/agenda-set-deadline)"
     ":PROPERTIES:"
     ":DATE: %U"
     ":END:")
    :children
    (("Chores" :keys "c"
      :headline "Chores"
      :tags ":home:errand:"
      )
     ("Errands" :keys "e"
      :headline "Errands"
      :tags ":home:errand:"))))

(defun idemacs/capture-personal-template ()
  "A list to hold the capture templates for personal tasks."
  '("Personal" :keys "i"
    :file idemacs-personal-path
    :children
    (("Reminders" :keys "r"
      :headline "Reminders"
      :template
      ("* TODO %^{Reminder} :personal:reminders:%^g"
       "DEADLINE: %(idemacs/agenda-set-deadline)"))
     ("Goals" :keys "g"
      :headline "Goals"
      :template
      ("* %^{Goal} :personal:goals:%^g"
       "%?"
       ":PROPERTIES:"
       ":DATE: %U"
       ":REASON: %^{Why is this a goal?}"
       ":END:"))
     ("Family" :keys "f"
      :headline "Family"
      :template
      ("* TODO %^{Activity} :personal:family:%^g"
       ":PROPERTIES:"
       ":DATE: %(idemacs/agenda-set-deadline)"
       ":LOCATION: %^{Location}"
       ":END:")))))

(provide 'org-setup)
;;; org-setup.el ends here
