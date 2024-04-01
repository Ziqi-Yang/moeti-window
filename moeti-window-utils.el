;;; moeti-window-utils.el --- utility functions for moeti-window  -*- lexical-binding: t; -*-
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

(defvar moeti-window-during-window-creation nil
  "TODO.")

(defalias 'moeti-window-get-id 'moeti-window-p)

(defun moeti-window-p (window)
  "Whether the given WINDOW is moeti-window.
Non-nil means the given window is moeti-window."
  (window-parameter window 'moeti-window))

(defun moeti-window-mark (window id)
  "Mark the given WINDOW as moeti-window.
This function will also give WINDOW an moeti-window ID.  ID shouldn't be nil.
The returned value is the WINDOW."
  (when id
    (set-window-parameter window 'moeti-window id))
  window)

(defun moeti-window-max-id (windows &optional moeti)
  "Return the max moeti-window id and its corresponding window in WINDOWS.
MOETI: whether all WINDOWS are moeti-windows.
Note that the moeti-windows' id must be number or marker object.
Return nil if there is no moeti-window in WINDOWS."
  (let ((ws (if moeti windows (moeti-window-filter windows)))
        id mid midw)
    (dolist (w ws)
      (if (not mid)
          (setq mid (moeti-window-get-id w)
                midw w)
        (setq id (moeti-window-get-id w))
        (when (> id mid)
          (setq mid id
                midw w))))
    (when mid
      (cons mid midw))))

(defun moeti-window-filter (windows &optional id)
  "Filter the given WINDOWS and get only moeti-windows.
WINDOWS: a list of window object.
If ID is non-nil, then exclude that moeti-window id."
  (seq-filter
   (if id
       (lambda (w)
         (let (mid)
           (setq mid (moeti-window-get-id w))
           (and mid (not (equal mid id)))))
     (lambda (w) (moeti-window-p w)))
   windows))

(defun moeti-window-count (windows)
  "Get the number of moeti-windows in WINDOWS."
  (seq-count (lambda (w) (moeti-window-p w)) windows))

(defun moeti-window-find (windows id)
  "Find moeti-window in WINDOWS by ID.
ID shouldn't be nil.
Return nil if not found."
  (when id
    (seq-find
     (lambda (w) (equal (moeti-window-get-id w) id))
     windows)))

(provide 'moeti-window-utils)

;;; moeti-window-utils.el ends here
