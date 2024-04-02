;;; moeti-window-defaults.el --- Default common functions used by moeti-window  -*- lexical-binding: t; -*-
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

(defun moeti-window-exclude-window-default-predicate (window)
  "Default function for judging whether a WINDOW is excluded.
Check buffer name regexp, derived modes.
You can customize `moeti-window-exclude-window-predicate-buffer-name-regexps'
and `moeti-window-exclude-window-predicate-derived-modes-list'."
  (let* ((b (window-buffer window))
         (bn (buffer-name b)))
    (catch 'res
      (dolist (re moeti-window-exclude-window-predicate-buffer-name-regexps)
        (when (string-match-p re bn)
          (throw 'res t)))
      (with-current-buffer b
        (derived-mode-p moeti-window-exclude-window-predicate-derived-modes-list)))))

(defun moeti-window-default-create-window (windows &rest _)
  "TODO.  SELECTED WINDOWS."
  ;; TODO how to restore window? will restored window still have moeti-window id?
  (let* ((max-id (or (car (moeti-window-max-id windows)) -1))
         w)
    (setq moeti-window-during-window-creation t)
    (unwind-protect
        (progn
          (setq w (moeti-window-mark (split-window-right) (1+ max-id)))
          (balance-windows))
      (setq moeti-window-during-window-creation nil))
    (list w)))

(defun moeti-window-default-destory-window (windows selected)
  "TODO.  WINDOWS SELECTED."
  ;; (message "> %s %s %s" windows selected (moeti-window-max-id (moeti-window-filter windows (moeti-window-get-id selected)) t))
  (when-let* ((id (moeti-window-get-id selected))
              (mws (moeti-window-filter windows id))
              (mid-w (moeti-window-max-id mws t))
              (mid (car mid-w))
              (w (cdr mid-w)))
    (when (> id mid)
      (moeti-window-mark selected (1- id)))
    (delete-window w)
    (balance-windows)
    (list w)))

(provide 'moeti-window-defaults)

;;; moeti-window-defaults.el ends here
