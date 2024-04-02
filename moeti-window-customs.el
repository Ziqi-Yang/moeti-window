;;; moeti-window-customs.el --- Customizable variables for Moeti Window  -*- lexical-binding: t; -*-
;; Copyright (C) 2024 Meow King <mr.meowking@anche.no>

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defgroup moeti-window nil
  "Multiple Window Editing."
  :prefix "moeti-window"
  :group 'editing)

(defcustom moeti-window-exclude-window-predicate-function 'moeti-window-exclude-window-default-predicate
  "A function to determine whether a window is excluded.
The function should take exactly one parameter: window.
The excluded windows will not be regarded as an judgment criteria for function
`moeti-window-create-window-predicate-function' and
`moeti-window-destory-window-predicate-function'."
  :type 'function
  :group 'moeti-window)

(defcustom moeti-window-exclude-window-predicate-buffer-name-regexps
  '("^\\*.*\\*$")
  "A list of regular expressions used for check whether a window is excluded."
  :type '(list string)
  :group 'moeti-window)

(defcustom moeti-window-exclude-window-predicate-derived-modes-list nil
  "A list of parent modes that is used for check whether a window is excluded."
  :type '(list symbol)
  :group 'moeti-window)

;; SHOULD RETURN CREATED WINDOW LISTS, nil if not created a window
(defcustom moeti-window-create-window-function 'moeti-window-default-create-window
  "TODO."
  :type 'function
  :group 'moeti-window)

;; SHOULD RETURN DESTORIED WINDOW LISTS, nil if not created a window
(defcustom moeti-window-destroy-window-function 'moeti-window-default-destory-window
  "TODO."
  :type 'function
  :group 'moeti-window)

(defcustom moeti-window-main-function 'moeti-window-cw-main
  "TODO."
  :type 'function
  :group 'moeti-window)

(provide 'moeti-window-customs)

;;; moeti-window-customs.el ends here
