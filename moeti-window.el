;;; moeti-window.el --- Multiple Window Editing -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Meow King <mr.meowking@anche.no>

;; Version: 0.1.0
;; Author: Meow King <mr.meowking@anche.no>
;; Keywords: convenience
;; URL: https://git.sr.ht/~meow_king/moeti-window
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

;; When there is only one window, automatically create other windows

;; Developer Note ==================
;; use `set-window-parameter' to hide tab-bar headline and mode-line
;; hook: `window-scroll-functions'

;;; Code:

(require 'moeti-window-customs)
(require 'moeti-window-defaults)
(require 'moeti-window-basic-window)
(require 'moeti-window-sync-window)
(require 'moeti-window-continuous-window)

(defconst moeti-window-version "0.1.0")

(defun moeti-window-scroll-function (window start)
  "Scroll function used for `window-scroll-functions'.
WINDOW: the window that triggers `window-scroll-functions';
START: the new window start position."
  (let ((windows (seq-filter
                  (lambda (w)
                    (and
                     (not (funcall
                           moeti-window-exclude-window-predicate-function w))))
                  (window-list))))
    (when (memq window windows)
      (funcall moeti-window-main-function windows window start))))

(defun moeti-window-pixel-scroll-function (&rest _)
  (let ((sw (selected-window)))
    (moeti-window-scroll-function sw (window-start sw))))

(define-minor-mode moeti-window-mode
  "El Psy Congroo."
  :global t
  :version moeti-window-version
  :lighter " Moeti"
  :group 'moeti-window
  (if moeti-window-mode
      (progn
        (add-hook 'window-scroll-functions 'moeti-window-scroll-function)
        (advice-add 'pixel-scroll-precision ':after 'moeti-window-pixel-scroll-function)
        ;; TODO emacs 29.1
        (advice-add 'pixel-scroll-precision-interpolate ':after 'moeti-window-pixel-scroll-function))
    (remove-hook 'window-scroll-functions 'moeti-window-scroll-function)
    (advice-remove 'pixel-scroll-precision 'moeti-window-pixel-scroll-function)
    (advice-remove 'pixel-scroll-precision-interpolate 'moeti-window-pixel-scroll-function)))

(provide 'moeti-window)

;;; moeti-window.el ends here
