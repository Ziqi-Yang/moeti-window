;;; moeti-window-sync-window.el --- Continuous Editing Experience for Multi Windows  -*- lexical-binding: t; -*-
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
(require 'cl-macs)

(defvar moeti-window-sw-during-sync nil
  "TODO.")

(defun moeti-window-sw-main (windows window _start)
  "TODO.
WINDOWS, WINDOW, START see `moeti-window-action-state-function'."
  (let ((ob (window-old-buffer window))
        (sw (selected-window)))
    ;; (message "#main function called: %s - %s - %s" windows window ob)
    (cond
     ((booleanp ob)  ; new window is created
      (unless (or moeti-window-during-window-creation (moeti-window-p window))
        (message "#moeti-window-sw-destory function called")
        (moeti-window-sw-destory windows)))
     ((and (length= windows 1) (not moeti-window-during-window-creation))
      (moeti-window-mark window 0)
      (message "#moeti-window-sw-create function called")
      (moeti-window-sw-create windows))
     ((and (not moeti-window-sw-during-sync) (eq sw window))
      (message "#moeti-window-sw-sync function called")
      (moeti-window-sw-sync windows)))))

(defun moeti-window-sw-create (windows)
  "Create second window used that continues the content of first window.
WINDOW, START and WINDOWS see `moeti-window-create-window-function'."
  (funcall moeti-window-create-window-function windows (selected-window)))

(defun moeti-window-sw-destory (windows)
  "Create second window used that continues the content of first window.
WINDOW, START and WINDOWS see `moeti-window-destroy-window-function'."
  (funcall moeti-window-destroy-window-function windows (selected-window)))

(defun moeti-window-sw-sync (windows)
  "TODO."
  (when-let* ((sw (selected-window))
              (swid (moeti-window-get-id sw))
              (mws (moeti-window-filter windows))
              (mws (seq-sort-by (lambda (w) (moeti-window-get-id w)) #'< mws))
              (sw-pos (seq-position mws sw)))
    ;; (message "%s %s" swid mws)
    (setq moeti-window-sw-during-sync t)
    (unwind-protect
        (progn
          (cl-loop for index from (1- sw-pos) downto 0
                   do
                   (set-window-start
                    (nth index mws)
                    (window-start (nth (1+ index) mws))))
          (cl-loop for index from (1+ sw-pos) to (1- (length mws))
                   do
                   (set-window-start
                    (nth index mws)
                    (window-start (nth (1- index) mws)))))
      (setq moeti-window-sw-during-sync nil))))

(provide 'moeti-window-sync-window)

;;; moeti-window-sync-window.el ends here
