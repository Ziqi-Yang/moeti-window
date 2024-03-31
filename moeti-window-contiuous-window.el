;;; moeti-window-contiuous-window.el --- Continuous Editing Experience for Multi Windows  -*- lexical-binding: t; -*-
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

(defvar moeti-window-cw-during-sync nil
  "TODO.")

(defun moeti-window-cw-main (windows window _start)
  "TODO.
WINDOWS, WINDOW, START see `moeti-window-action-state-function'."
  (let ((ob (window-old-buffer window))
        (sw (selected-window)))
    ;; (message "#main function called: %s - %s - %s" windows window ob)
    (cond
     ((booleanp ob)  ; new window is created
      (unless (or moeti-window-during-window-creation (moeti-window-p window))
        (message "#moeti-window-cw-destory function called")
        (moeti-window-cw-destory windows)))
     ((and (length= windows 1) (not moeti-window-during-window-creation))
      (moeti-window-mark window 0)
      (message "#moeti-window-cw-create function called")
      (moeti-window-cw-create windows))
     ((and (not moeti-window-cw-during-sync) (eq sw window))
      (message "#moeti-window-cw-sync function called")
      (moeti-window-cw-sync windows)))))

(defun moeti-window-cw-create (windows)
  "Create second window used that continues the content of first window.
WINDOW, START and WINDOWS see `moeti-window-create-window-function'."
  (funcall moeti-window-create-window-function windows (selected-window)))

(defun moeti-window-cw-destory (windows)
  "Create second window used that continues the content of first window.
WINDOW, START and WINDOWS see `moeti-window-destroy-window-function'."
  (funcall moeti-window-destroy-window-function windows (selected-window)))

(defun moeti-window-cw-sync (windows)
  "TODO."
  (when-let* ((sw (selected-window))
              (swid (moeti-window-get-id sw))
              (mws (moeti-window-filter windows))
              (mws (seq-sort-by (lambda (w) (moeti-window-get-id w)) #'< mws))
              (sw-pos (seq-position mws sw)))
    ;; (message "%s %s" swid mws)
    (setq moeti-window-cw-during-sync t)
    (cl-loop for index from (1- sw-pos) downto 0
             do
             (set-window-start
              (nth index mws)
              ;; TODO
              (window-start (nth (1+ index) mws))))
    (cl-loop for index from (1+ sw-pos) to (1- (length mws))
             do
             (let ((prev-window (nth (1- index) mws))
                   (cur-window (nth index mws)))
               (set-window-start
                cur-window
                ;; (1+ (window-end prev-window t))
                ;; TODO
                (with-current-buffer (window-buffer prev-window)
                  (save-excursion
                    (goto-char (window-end prev-window t))
                    (forward-line -3)
                    (point))))
               ;; (message "%s: %s" (1+ (window-end prev-window t)) (window-start cur-window))
               ))
    (setq moeti-window-cw-during-sync nil)))

(provide 'moeti-window-contiuous-window)

;;; moeti-window-sync-window.el ends here
