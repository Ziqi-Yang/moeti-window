;;; dual-window.el --- Dual Window Editing -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Meow King <mr.meowking@anche.no>

;; Version: 0.1.0
;; Author: Meow King <mr.meowking@anche.no>
;; Keywords: convenience
;; URL: https://git.sr.ht/~meow_king/dual-window
;; License: GNU General Public License >= 3
;; Package-Requires: ()  ;FIXME: `package-lint-current-buffer'

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

;; When there is only one window, automatically create another window


;; Developer Note ==================
;; use `set-window-parameter' to hide tab-bar headline and mode-line
;; hook: `window-scroll-functions'

;;; Code:

(defgroup dual-window nil
  "Dual Window Editing."
  :prefix "dual-window"
  :group 'editing)

(defconst dual-window-version "0.1.0")

(defcustom dual-window-exclude-window-predicate-function 'dual-window-exclude-window-default-predicate
  "A function to determine whether a window is excluded.
The function should take exactly one parameter: window.
The excluded windows will not be regarded as an judgment criteria for function
`dual-window-create-window-predicate-function' and
`dual-window-destory-window-predicate-function'."
  :type 'function
  :group 'dual-window)

(defcustom dual-window-action-state-function 'dual-window-sync-window-action-state
  "TODO."
  :type 'function
  :group 'dual-window)

(defcustom dual-window-create-window-function 'dual-window-create-sync-window
  "Function used for create a dual window when there is only one window.
The function should accepts three parameters: window and display start, and
windows.  The first two of which is the same as the function parameters
specified in `window-scroll-functions'.  The last parameter is the live windows
that is not excluded."
  :type 'function
  :group 'dual-window)

(defcustom dual-window-destroy-window-function 'dual-window-destroy-sync-window
  "Function used for destroy previously created dual window.
The function should accepts three parameters: window and display start, and
windows.  The first two of which is the same as the function parameters
specified in `window-scroll-functions'.  The last parameter is the live windows
that is not excluded."
  :type 'function
  :group 'dual-window)

(defcustom dual-window-fallback-action-funciton nil
  "TODO."
  :type '(choice (function :tag "Function")
                 (const :tag "Do nothing" nil))
  :group 'dual-window)

(defcustom dual-window-exclude-window-predicate-buffer-name-regexps nil
  "A list of regular expressions used for check whether a window is excluded."
  :type '(list string)
  :group 'dual-window)

(defcustom dual-window-exclude-window-predicate-derived-modes-list nil
  "A list of parent modes that is used for check whether a window is excluded."
  :type '(list symbol)
  :group 'dual-window)

(defun dual-window-exclude-window-default-predicate (window)
  "Default function for judging whether a WINDOW is excluded.
Check buffer name regexp, derived modes.
You can customize `dual-window-exclude-window-predicate-buffer-name-regexps'
and `dual-window-exclude-window-predicate-derived-modes-list'."
  (let* ((b (window-buffer window))
         (bn (buffer-name b)))
    (catch 'res
      (dolist (re dual-window-exclude-window-predicate-buffer-name-regexps)
        (when (string-match-p re bn)
          (throw 'res t)))
      (with-current-buffer b
        (derived-mode-p dual-window-exclude-window-predicate-derived-modes-list)))))

(defun dual-window-action-state-function (windows window start)
  "TODO.
WINDOWS, WINDOW, START see `dual-window-action-state-function'."
  )

(defun dual-window-create-sync-window (window start &rest _)
  "Create second window used that continues the content of first window.
WINDOW, START see `dual-window-create-window-function'."
  )


(defun dual-window-destroy-sync-window (window start &rest _)
  "Destroy second window used that continues the content of first window.
WINDOW, START see `dual-window-destroy-window-function'."
  )


(defun dual-window-scroll-function (window start)
  "Scroll function used for `window-scroll-functions'.
WINDOW: the window that triggers `window-scroll-functions';
START: the new window start position."
  (let ((windows (seq-filter
                  (lambda (w)
                    (not (funcall dual-window-exclude-window-predicate-function w)))
                  (window-list))))
    (pcase (funcall dual-window-action-state-function windows window start)
      ('create
       (funcall dual-window-create-window-function window start windows))
      ('destory
       (funcall dual-window-destroy-window-function window start windows))
      (_ (when dual-window-fallback-action-funciton
           (funcall dual-window-fallback-action-funciton window start))))))

(provide 'dual-window)

;;; dual-window.el ends here
