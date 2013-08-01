;;; ob-php.el --- org-babel functions for php evaluation

;; Copyright (C) steckerhalter

;; Author: steckerhalter
;; Keywords: literate programming, reproducible research, php
;; Homepage: https://github.com/steckerhalter/ob-php

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating php source code.

;;; Requirements:

;; php-cli installation

;;; Code:
(require 'ob)
(require 'ob-eval)

(add-to-list 'org-babel-tangle-lang-exts '("php"))

(defvar org-babel-default-header-args:php '())

(defun org-babel-execute:php (body params)
  "Execute a block of PHP code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
         (file (cdr (assoc :file params)))
         (cmdline (cdr (assoc :cmdline params)))
         (cmd (concat "php " (or cmdline "")
                      "-r '" (org-babel-expand-body:generic body params)"'")))
         (org-babel-eval cmd "")))

(defun org-babel-prep-session:php (session params)
  "Raise an error because PHP does not support sessions."
  (error "PHP does not support sessions"))

(provide 'ob-php)
;;; ob-php.el ends here
