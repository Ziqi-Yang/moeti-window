;;; moeti-window-basic-window.el --- Basic Window Management for moeti-window   -*- lexical-binding: t; -*-
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

(require 'moeti-window-customs)
(require 'moeti-window-utils)

(defun moeti-window-bw-main (windows window _start)
  "TODO.
WINDOWS, WINDOW, START see `moeti-window-action-state-function'."
  (let ((ob (window-old-buffer window)))
    ;; (message "#main function called: %s - %s - %s" windows window ob)
    (cond
     ((booleanp ob)  ; new window is created
      (unless (or moeti-window-during-window-creation (moeti-window-p window))
        (message "#moeti-window-bw-destory function called")
        (moeti-window-bw-destory windows)))
     ((and (length= windows 1) (not moeti-window-during-window-creation))
      (moeti-window-mark window 0)
      (message "#moeti-window-bw-create function called")
      (moeti-window-bw-create windows))
     (t nil))))

(defun moeti-window-bw-create (windows)
  "Create second window used that continues the content of first window.
WINDOW, START and WINDOWS see `moeti-window-create-window-function'."
  (funcall moeti-window-create-window-function windows (selected-window)))

(defun moeti-window-bw-destory (windows)
  "Create second window used that continues the content of first window.
WINDOW, START and WINDOWS see `moeti-window-destroy-window-function'."
  (funcall moeti-window-destroy-window-function windows (selected-window)))


(provide 'moeti-window-basic-window)

;;; moeti-window-basic-window.el ends here
