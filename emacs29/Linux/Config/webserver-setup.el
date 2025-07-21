;;; webserver-setup.el --- settings for creating and testing websites.  -*- lexical-binding: t; -*-

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

;; This file will contain the packages and variables to set up and view a local webserver.

;;; Code:
(use-package simple-httpd
  :ensure t)

(provide 'webserver-setup)
;;; webserver-setup.el ends here
